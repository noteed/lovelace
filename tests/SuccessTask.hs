-- | Example workflow with a single Task activity.
module SuccessTask where

import System.Environment (getArgs)

import Lovelace
import StringWorkflows


--------------------------------------------------------------------------------
main :: IO ()
main = do
  [arg] <- getArgs
  (ss, s) <- runs handler () workflow () arg
  writeFile "success-task.log" (unlines $
    [ "Input " ++ show arg ]
    ++ concatMap texts ss
    ++ [ "End of workflow." ])
  writeFile "success-task.dot" (graphviz workflow Nothing)


--------------------------------------------------------------------------------
workflow :: Workflow () String String String
workflow = Workflow "succeed" single [] [single]

single = Activity "SUCCEED"
  "Running SUCCEED..."
  (Task "return-success")

handler name s k = do
  return (s, "SUCCESS")
