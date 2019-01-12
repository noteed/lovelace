-- | Simple workflow engine. Just playing around...
--
-- Type variable mnemonic: `o`, `t`, `k`, `g` stands respectively for object,
-- task, token, tag.
--
-- A workflow can be seen as a function Token -> IO Token associated to an
-- object.
-- It receives an initial token (which carries data), processes it, possibly
-- mutating the object, and returns a new token. By convention, a token
-- different than "SUCCESS" is seen as a failure.
--
-- A workflow is simply a directed graph: nodes linked together by arcs.
-- Nodes can run activities, and arcs are labeled by tags.
--
-- When an object enters a workflow, it starts at the workflow's initial node.
-- It then moves along the arcs until it reaches a final node. At each node,
-- an activity can be run. The activity can change the object state or can
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
-- Activities can be pure or tasks : pure activities are defined by the
-- workflow user while tasks are defined and handled by the engine. Also,
-- activities' state is within the object, while tasks receive and output their
-- own data.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lovelace where

import Data.Aeson
import Data.Function (on)
import qualified Data.HashMap.Strict as H
import Data.List (nubBy)
import qualified Data.Text as T

-- | Activities manipulates objects. They are parametrized by object, task
-- and token types.
data Activity o t k = Activity
  { activityName :: String
    -- ^ An activity is identified (within a given workflow) by its name.
  , activityDescription :: String
    -- ^ Human-friendly description of the activity.
  , activityHandler :: TaskOrPure o t k
    -- What the activity does when run.
  }

instance Show (Activity o t k) where
  show Activity{..} = activityName

-- | Request the engine to do some asynchronous task. Once the task is
-- completed, the engine will supply a regular token to continue the
-- workflow.
class Task t where
  serializeTask :: t -> String

-- | In the Pure case, given an object (or state) and a token, compute a new
-- state and return a new token, or in the Task case, just name the task.
-- The task is processed by the workflow engine. It is recommanded to engine
-- implementors to export only the input token to the task instead of the full
-- current state. I.e. the task can be seen as a function (k -> IO k).
data TaskOrPure o t k = Task t | Pure (o -> k -> (o, k))

-- | Represent the result of an activity: either a token, or a task. In that
-- case, the input token is provided.
data TaskOrToken t k = Task' k t | Token k
  deriving Show

class Token k g where
  tag :: k -> g

-- | Activities are linked together into a workflow.
-- TODO Check activity names are unique.
-- Different engines for different workflow types can be offered by this
-- library, or constructed by users.
-- Workflows are parametrized by object, task, token and tag types.
data Workflow o t k g = Workflow
  { workflowName :: String
  , workflowInitial :: Activity o t k
  , workflowTransitions :: [(Activity o t k, g, Activity o t k)]
  , workflowFinal :: [Activity o t k]
  }
  deriving Show

-- | `Step` represents the object after an activity has been done, and before
-- the transition has been followed. This also represent a "more complete"
-- object, i.e. which includes its workflow-related state. See the `serialize`
-- function below.
-- TODO Include the input token in the Step representation.
data Step o t k g = Step
  { stepWorkflow :: Workflow o t k g
  , stepActivity :: Activity o t k
  , stepObject :: o
  , stepResult :: TaskOrToken t k
  }
  deriving Show

final :: Workflow o t k g -> Activity o t k -> Bool
final w a = activityName a `elem` map activityName (workflowFinal w)

-- | Start a workflow, performing a single step. Use `run` if the whole
-- workflow must be traversed directly.
start :: Workflow o t k g -> o -> k -> Step o t k g
start w@Workflow{..} r k = step w workflowInitial r k

-- | Continue a workflow, after some steps have been done.
continue :: (Token k g, Show k, Eq g) =>
  Workflow o t k g -> Activity o t k -> o -> k -> Step o t k g
continue w@Workflow{..} a r' t' = do
  case lookupActivity a t' workflowTransitions of
    Nothing -> error $ "No such transition.\nActivity: " ++ activityName a
      ++ "\nTransition: " ++ show t'
    Just a' -> step w a' r' t'

-- | Perform a single step in the workflow, i.e. compute the task or token
-- returned by the activity (but "stays in" the activity).
step :: Workflow o t k g -> Activity o t k -> o -> k -> Step o t k g
step w@Workflow{..} a r k =
  case activityHandler a of
    Pure f -> let (r', k') = f r k
              in Step w a r' (Token k')
    Task t -> Step w a r (Task' k t)

-- | Run a workflow, from start to finish.
-- Running a workflow steps through the activities and handle tasks fired by
-- activities, if any.
-- The function to run a task can modify the engine state.
run :: (Eq g, Show k, Token k g, Monad m) =>
  (t -> s -> k -> m (s, k)) -> s -> Workflow o t k g -> o -> k -> m (Step o t k g, s)
run handler engineState w r k = do
  (ss, s) <- runs handler engineState w r k
  return (last ss, s)

-- | Return all the steps.
runs :: (Eq g, Show k, Token k g, Monad m) =>
  (t -> s -> k -> m (s, k)) -> s -> Workflow o t k g -> o -> k -> m ([Step o t k g], s)
runs handler engineState w r k = run_ [] handler engineState w r k

-- | Return all the steps.
-- The accumulator is used to remember all the past steps.
run_ :: (Eq g, Show k, Token k g, Monad m) =>
  [Step o t k g] -> (t -> s -> k -> m (s, k)) -> s
  -> Workflow o t k g -> o -> k -> m ([Step o t k g], s)
run_ acc handler engineState w r k = loop acc engineState (start w r k)

  where

  loop acc s (Step _ a r' t) = do
    -- Run the task, if any, returned by the step.
    (s', t') <- case t of
      Task' k task -> handler task s k
      Token k' -> return (s, k')
    let acc' = Step w a r' (Token t') : acc
    if final w a
      then return (reverse acc', s)
      else loop acc' s' (continue w a r' t')

-- | Run a workflow as far as possible but without processing tasks.
-- In other words, continue as long as activities result in tokens.
-- Once a task is reached or a final activity is reached, this stops.
run' :: (Eq g, Show k, Token k g) => Step o t k g -> Step o t k g
run' s@(Step w a r t) =
  if final w a
    then s
    else case t of
      Task' _ _ -> s
      Token k' -> run' (continue w a r k')

-- | Find the next activity, given the current activity and a token.
lookupActivity :: (Eq g, Token k g) =>
  Activity o t k -> k -> [(Activity o t k, g, Activity o t k)] -> Maybe (Activity o t k)
lookupActivity _ _ [] = Nothing
lookupActivity a t ((b, t' , b''):ts)
  | activityName a == activityName b && tag t == t' = Just b''
  | otherwise = lookupActivity a t ts

-- | Return all the activities of a workflow.
activities :: Workflow o t k g -> [(String, Activity o t k)]
activities Workflow{..} = map (\a -> (activityName a, a))
  $ nubBy ((==) `on` activityName)
  $ map fst3 workflowTransitions ++ map thd3 workflowTransitions

-- | The workflow state tied to a object can be saved in the object itself.
-- This means that given a workflow definition and an object, it is possible
-- to continue to step the object through the workflow. All the state is
-- self-contained.
-- This is an example function for `o` instanciated to aeson's Object.
serialize :: Task t => Step Object t k g -> Object
serialize (Step w a r t) =
  H.insert "workflow_name" (f $ workflowName w)
  . H.insert "current_activity" (f $ activityName a)
  . (case t of { Task' _ x -> H.insert "waiting_task" (f $ serializeTask x) ; _ -> H.delete "waiting_task" })
  $ r
  where f = String . T.pack

fst3 (a, _, _) = a
thd3 (_, _, c) = c

--------------------------------------------------------------------------------
-- Graphviz.
--------------------------------------------------------------------------------

-- This requires the tag type is an instance of Show.
graphviz :: Show g => Workflow o t k g -> Maybe (Activity o t k) -> String
graphviz Workflow{..} ma = unlines $
  [ "digraph " ++ workflowName ++ " {"
  , "rankdir=LR;"
  , "size=\"8,5\""
  , "node [shape = Mcircle];"
  ] ++
  maybe [] (return . activityName) ma ++
  ["node [shape = doublecircle];"
  , unwords (map activityName (workflowInitial : workflowFinal)) ++ ";"
  , "node [shape = circle];"
  ] ++
  map f workflowTransitions ++
  [ "}"
  ]

  where

  f (a, b, c) =
    activityName a ++ " -> " ++ activityName c ++ " [ label = " ++ show b ++ " ];"

graphvizs Step{..} = graphviz stepWorkflow (Just stepActivity)
