-- | Example workflow with a single Task activity.
module SuccessTask where

import Lovelace
import StringWorkflows


--------------------------------------------------------------------------------
main :: IO ()
main = run handler () workflow () "START" >>= print


--------------------------------------------------------------------------------
workflow :: Workflow () String String String
workflow = Workflow "task" single [] [single]

single = Activity "A"
  "Running A..."
  (Task "task-a")

handler name s k = do
  return (s, "SUCCESS")
