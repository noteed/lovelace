-- | Simple workflow engine. Just playing around...
--
-- Type variable mnemonic: `o`, `t`, `k`, `g` stands respectively for object,
-- task, token, tag.
--
-- A workflow is simply a directed graph: nodes linked together by arcs.
-- Nodes can run activities, and arcs are labeled by tags.
--
-- When an object enters a workflow, it starts at the workflow's initial node.
-- It then moves along the arcs until it reaches a final node. At each node,
-- an activity can be run. The activity can change the object state and can
-- start a task. The workflow engine handles the task and the object remains
-- at that node until the task is completed. The task result is a token. The
-- token is used to choose an ongoing arc, and move the object to the pointed
-- node. The cycle repeats at each node.
--
-- The disctinction between tokens and tags is used so that choosing an arc is
-- just a matter of comparing two tags: the one drawn from the token, and the
-- one labelling the arc. The token in addition to carrying a tag can hold
-- values used by the next node.
--
-- Activities and tasks are different beasts: activities are defined by the
-- workflow user while tasks are defined and handled by the engine. Also,
-- activities' state is within the record, while tasks can carry additional
-- data (and different data from tasks to tasks) that do not make sense on the
-- record itself.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lovelace where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- | Activities manipulates objects. They are parametrized by object, task
-- and token types.
data Activity o t k = Activity
  { activityName :: String
    -- ^ An activity is identified (within a given workflow) by its name.
  , activityDescription :: String
    -- ^ Human-friendly description of the activity.
  , activityHandler :: o -> k -> (o, TaskOrToken t k)
    -- ^ Given a record (or state) and a token, compute a new state
    -- and return a new token or a task (which will generate a token).
  }

instance Show (Activity o t k) where
  show Activity{..} = activityName

-- | Request the engine to do some asynchronous task. Once the task is
-- completed, the engine will supply a regular token to continue the
-- workflow.
class Task t where
  serializeTask :: t -> String

-- | Similar to `Either` but makes things more clear.
data TaskOrToken t k = Task t | Token k
  deriving Show

class Token k g where
  tag :: k -> g

-- | Activities are linked together into a workflow.
-- TODO Check activity names are unique.
-- TODO A workflow should be parametrized by records, token and tasks.
-- Different engines for different workflow types can be offered by this
-- library, or constructed by users.
-- Workflows are parametrized by task and token types.
data Workflow o t k g = Workflow
  { workflowName :: String
  , workflowInitial :: Activity o t k
  , workflowTransitions :: [((Activity o t k, g), Activity o t k)]
  , workflowFinal :: [Activity o t k]
  }
  deriving Show

-- | `Step` represents the record after an activity has been done, and before
-- the transition has been followed. This also represent a "more complete"
-- record, i.e. which includes its workflow-related state. See the `serialize`
-- function below.
data Step o t k g = Step (Workflow o t k g) (Activity o t k) o (TaskOrToken t k)
  deriving Show

-- | Start a workflow, performing a single step. Use `run` if the whole
-- workflow must be traversed directly.
start w@Workflow{..} r = step w workflowInitial r undefined
  -- TODO Should a workflow specify an initial token ?

-- | Continue a workflow, after some steps have been done.
continue w@Workflow{..} a r' t' = do
  case lookupActivity a t' workflowTransitions of
    Nothing -> error $ "No such transition.\nActivity: " ++ activityName a
      ++ "\nTransition: " ++ show t'
    Just a' -> step w a' r' t'

-- | Perform a single step in the workflow.
step w@Workflow{..} a r k =
  let (r', t') = activityHandler a r k
  in Step w a r' t'

-- | Run a workflow, from start to finish.
-- Running a workflow steps through the activities and handle tasks fired by
-- activities, if any.
-- The function to run a task can modify the engine state.
run :: (Eq g, Show k, Token k g) =>
  (s -> t -> IO (s, k)) -> s -> Workflow o t k g -> o -> IO (Step o t k g)
run runTask engineState w r = do
  putStr . activityName . workflowInitial $ w
  putStr " - "
  putStrLn $ activityDescription . workflowInitial $ w
  loop engineState $ start w r

  where

  final x = activityName x `elem` map activityName (workflowFinal w)
  loop s (Step _ a r' t) = do
    (s', t') <- case t of
      Task task -> runTask s task
      Token t' -> return (s, t')
    if final a
      then return $ Step w a r' (Token t')
      else do
        putStr $ activityName a
        putStr " - "
        putStrLn $ activityDescription a
        loop s' $ continue w a r' t'

-- | Find the next activity, given the current activity and a token.
lookupActivity :: (Eq g, Token k g) =>
  Activity o t k -> k -> [((Activity o t k, g), Activity o t k)] -> Maybe (Activity o t k)
lookupActivity _ _ [] = Nothing
lookupActivity a t (((b,t'),b''):ts)
  | activityName a == activityName b && tag t == t' = Just b''
  | otherwise = lookupActivity a t ts

-- | The workflow state tied to a record can be saved in the record itself.
-- This means that given a workflow definition and a record, it is possible
-- to continue to step the record through the workflow. All the state is
-- self-contained.
-- This is an example function for `o` instanciated to aeson's Object.
serialize :: Task t => Step Object t k g -> Object
serialize (Step w a r t) =
  H.insert "workflow_name" (f $ workflowName w)
  . H.insert "current_activity" (f $ activityName a)
  . (case t of { Task x -> H.insert "waiting_task" (f $ serializeTask x) ; _ -> H.delete "waiting_task" })
  $ r
  where f = String . T.pack
