-- | Simple workflow engine. Just playing around...
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- | Run the example workflow.
main :: IO ()
main = do
  s <- run workflow (record [])
  print s
  print $ serialize s

-- | Activities manipulates objects.
data Activity = Activity
  { activityName :: String
    -- ^ An activity is identified (within a given workflow) by its name.
  , activityDescription :: String
    -- ^ Human-friendly description of the activity.
  , activityHandler :: Object -> IO (Object, TaskOrToken)
    -- ^ Given a record (or state) (TODO and a token), compute a new state
    -- and return a new token or a task (which will generate a token).
  }

instance Show Activity where
  show Activity{..} = activityName

-- | Transitions are identified by a Token. Currently a token is simply a
-- string but this should be replaced by an object.
type Token = String

-- | Request the engine to do some asynchronous task. Once the task is
-- completed, the engine will supply a regular token to continue the
-- workflow.
type Task = String

-- | Similar to `Either` but makes things more clear.
data TaskOrToken = Task String | Token String
  deriving Show

-- | Activities are linked together into a workflow.
-- TODO Check activity names are unique.
-- TODO A workflow should be parametrized by records, token and tasks.
-- Different engines for different workflow types can be offered by this
-- library, or constructed by users.
data Workflow = Workflow
  { workflowName :: String
  , workflowInitial :: Activity
  , workflowTransitions :: [((Activity, Token), Activity)]
  , workflowFinal :: [Activity]
  }
  deriving Show

-- | `Step` represents the record after an activity has been done, and before
-- the transition has been followed.
data Step = Step Workflow Activity Object (TaskOrToken)
  deriving Show

-- | Run an activity handler on a record.
runActivity Activity{..} r = do
  putStr activityName
  putStr " - "
  putStrLn activityDescription
  activityHandler r

-- | Start a workflow, performing a single step. Use `run` if the whole
-- workflow must be traversed directly.
start w@Workflow{..} r = step w workflowInitial r

-- | Continue a workflow, after some steps have been done.
continue w@Workflow{..} a r' t' = do
  case lookupActivity a t' workflowTransitions of
    Nothing -> error "No such transition."
    Just a' -> step w a' r'

-- | Perform a single step in the workflow.
step w@Workflow{..} a r = do
  (r', t') <- runActivity a r
  return $ Step w a r' t'

-- | Run a workflow, from start to finish.
-- Running a workflow steps through the activities and handle tasks fired by
-- activities, if any.
run w r = do
  current <- start w r
  loop current

  where

  final x = activityName x `elem` map activityName (workflowFinal w)
  loop current = case current of
    Step _ a r' (Task task)
      | final a -> runTask task >>= return . Step w a r' . Token
      | otherwise -> runTask task >>= continue w a r' >>= loop
    Step _ a r' (Token t')
      | final a -> return current
      | otherwise -> continue w a r' t' >>= loop

runTask name = do
  putStrLn $ "Running task " ++ name ++ "..."
  return "FINAL"

-- | Find the next activity, given the current activity and a token.
lookupActivity :: Activity -> Token -> [((Activity, Token), Activity)] -> Maybe Activity
lookupActivity _ _ [] = Nothing
lookupActivity a t (((b,t'),b''):ts)
  | activityName a == activityName b && t == t' = Just b''
  | otherwise = lookupActivity a t ts

-- | The workflow state tied to a record can be saved in the record itself.
-- This means that given a workflow definition and a record, it is possible
-- to continue to step the record through the workflow. All the state is
-- self-contained.
serialize :: Step -> Object
serialize (Step w a r t) =
  H.insert "workflow_name" (f $ workflowName w)
  . H.insert "current_activity" (f $ activityName a)
  . (case t of { Task x -> H.insert "waiting_task" (f x) ; _ -> H.delete "waiting_task" })
  $ r
  where f = String . T.pack

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int = Number . I

initial = Activity "INIT"
  "Initialization..." $ \_ -> do
  return (record [("count", int 0)], Token "SECOND")

second = Activity "SECOND"
  "Get input (`bye` to exit)..." $ \state -> do
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
      state' = record [("count", int (count + 1))]
  line <- getLine
  if line == "bye"
    then return (state', Token "BYE")
    else do
      putStrLn line
      return (state', Token "SECOND")

third = Activity "THIRD"
  "Will wait for task completion..." $ \state -> do
  return (state, Task "TASK")

final = Activity "FINAL"
  "End of workflow." $ \state -> do
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
  putStrLn $ "Count: " ++ show count
  return (state, Token "STOP")

transitions = [
    ((initial, "SECOND"), second),
    ((second, "SECOND"), second),
    ((second, "BYE"), third),
    ((third, "FINAL"), final)
  ]

workflow = Workflow "example" initial transitions [final]
