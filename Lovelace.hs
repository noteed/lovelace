-- | Simple workflow engine. Just playing around...
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lovelace where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- | Activities manipulates objects. They are parametrized by task and token
-- types.
data Activity t k = Activity
  { activityName :: String
    -- ^ An activity is identified (within a given workflow) by its name.
  , activityDescription :: String
    -- ^ Human-friendly description of the activity.
  , activityHandler :: Object -> (Object, TaskOrToken t k)
    -- ^ Given a record (or state) (TODO and a token), compute a new state
    -- and return a new token or a task (which will generate a token).
  }

instance Show (Activity t k) where
  show Activity{..} = activityName

-- | Request the engine to do some asynchronous task. Once the task is
-- completed, the engine will supply a regular token to continue the
-- workflow.
class Task t where
  serializeTask :: t -> String

-- | Similar to `Either` but makes things more clear.
data TaskOrToken t k = Task t | Token k
  deriving Show

-- | Activities are linked together into a workflow.
-- TODO Check activity names are unique.
-- TODO A workflow should be parametrized by records, token and tasks.
-- Different engines for different workflow types can be offered by this
-- library, or constructed by users.
-- Workflows are parametrized by task and token types.
data Workflow t k = Workflow
  { workflowName :: String
  , workflowInitial :: Activity t k
  , workflowTransitions :: [((Activity t k, k), Activity t k)]
  , workflowFinal :: [Activity t k]
  }
  deriving Show

-- | `Step` represents the record after an activity has been done, and before
-- the transition has been followed. This also represent a "more complete"
-- record, i.e. which includes its worflow-related state. See the `serialize`
-- function below.
data Step t k = Step (Workflow t k) (Activity t k) Object (TaskOrToken t k)
  deriving Show

-- | Run an activity handler on a record.
runActivity Activity{..} r = do
  putStr activityName
  putStr " - "
  putStrLn activityDescription
  return $ activityHandler r

-- | Start a workflow, performing a single step. Use `run` if the whole
-- workflow must be traversed directly.
start w@Workflow{..} r = step w workflowInitial r

-- | Continue a workflow, after some steps have been done.
continue w@Workflow{..} a r' t' = do
  case lookupActivity a t' workflowTransitions of
    Nothing -> error $ "No such transition.\nActivity: " ++ activityName a
      ++ "\nTransition: " ++ t'
    Just a' -> step w a' r'

-- | Perform a single step in the workflow.
step w@Workflow{..} a r = do
  (r', t') <- runActivity a r
  return $ Step w a r' t'

-- | Run a workflow, from start to finish.
-- Running a workflow steps through the activities and handle tasks fired by
-- activities, if any.
-- The function to run a task can modify the engine state.
run runTask engineState w r = start w r >>= loop engineState

  where

  final x = activityName x `elem` map activityName (workflowFinal w)
  loop s (Step _ a r' t) = do
    (s', t') <- case t of
      Task task -> runTask s task
      Token t' -> return (s, t')
    if final a
      then return $ Step w a r' (Token t')
      else continue w a r' t' >>= loop s'

-- | Find the next activity, given the current activity and a token.
lookupActivity :: Eq k =>
  Activity t k -> k -> [((Activity t k, k), Activity t k)] -> Maybe (Activity t k)
lookupActivity _ _ [] = Nothing
lookupActivity a t (((b,t'),b''):ts)
  | activityName a == activityName b && t == t' = Just b''
  | otherwise = lookupActivity a t ts

-- | The workflow state tied to a record can be saved in the record itself.
-- This means that given a workflow definition and a record, it is possible
-- to continue to step the record through the workflow. All the state is
-- self-contained.
serialize :: Task t => Step t k -> Object
serialize (Step w a r t) =
  H.insert "workflow_name" (f $ workflowName w)
  . H.insert "current_activity" (f $ activityName a)
  . (case t of { Task x -> H.insert "waiting_task" (f $ serializeTask x) ; _ -> H.delete "waiting_task" })
  $ r
  where f = String . T.pack
