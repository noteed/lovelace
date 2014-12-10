-- | Simple workflow engine. Just playing around...
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as H

-- | Run the example workflow.
main :: IO ()
main = run workflow (record []) >>= print

-- | Activities manipulates objects.
data Activity = Activity
  { activityName :: String
    -- ^ An activity is identified (within a given workflow) by its name.
  , activityDescription :: String
    -- ^ Human-friendly description of the activity.
  , activityHandler :: Object -> IO (Object, Token)
    -- ^ Given a record (or state) (TODO and a token), compute a new state
    -- and return a new token.
  }

instance Show Activity where
  show Activity{..} = activityName

-- | Transitions are identified by a Token. Currently a token is simply a
-- string but this should be replaced by an object.
type Token = String

-- | Activities are linked together into a workflow.
-- TODO Check activity names are unique.
data Workflow = Workflow
  { workflowInitial :: Activity
  , workflowTransitions :: [((Activity, Token), Activity)]
  , workflowFinal :: [Activity]
  }

-- | `Step` represents the record after an activity has been done, and before
-- the transition has been followed.
data Step =
    Step Activity Object Token
  | Final Activity Object Token
  deriving Show

-- | Run an activity handler on a record.
runActivity Activity{..} s = do
  putStr activityName
  putStr " - "
  putStrLn activityDescription
  activityHandler s

-- | Start a workflow, performing a single step. Use `run` if the whole
-- workflow must be traversed directly.
start w@Workflow{..} s = step w workflowInitial s

-- | Continue a workflow, after some steps have been done.
continue w@Workflow{..} a s' t' = do
  case lookupActivity a t' workflowTransitions of
    Nothing -> error "No such transition."
    Just a' -> step w a' s'

-- | Perform a single step in the workflow.
step w@Workflow{..} a s = do
  (s', t') <- runActivity a s
  let current =
        if activityName a `elem` map activityName workflowFinal
        then Final
        else Step
  return $ current a s' t'

-- | Run a workflow, from start to finish.
run w s = do
  current <- start w s
  loop current

  where

  loop current = case current of
    Step a s' t' -> continue w a s' t' >>= loop
    Final _ s' _ -> return s'

-- | Find the next activity, given the current activity and a token.
lookupActivity :: Activity -> Token -> [((Activity, Token), Activity)] -> Maybe Activity
lookupActivity _ _ [] = Nothing
lookupActivity a t (((b,t'),b''):ts)
  | activityName a == activityName b && t == t' = Just b''
  | otherwise = lookupActivity a t ts

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int = Number . I

initial = Activity "INIT"
  "Initialization..." $ \_ -> do
  return (record [("count", int 0)], "SECOND")

second = Activity "SECOND"
  "Get input (`bye` to exit)..." $ \state -> do
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
      state' = record [("count", int (count + 1))]
  line <- getLine
  if line == "bye"
    then return (state', "BYE")
    else do
      putStrLn line
      return (state', "SECOND")

third = Activity "THIRD"
  "Will wait for task completion (TODO)..." $ \state -> do
  return (state, "FINAL")

final = Activity "FINAL"
  "End of workflow." $ \state -> do
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
  putStrLn $ "Count: " ++ show count
  return (state, "STOP")

transitions = [
    ((initial, "SECOND"), second),
    ((second, "SECOND"), second),
    ((second, "BYE"), third),
    ((third, "FINAL"), final)
  ]

workflow = Workflow initial transitions [final]
