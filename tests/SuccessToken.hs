-- | Example workflow with a single Pure activity.
module SuccessToken where

import System.Environment (getArgs)

import Lovelace
import StringWorkflows


--------------------------------------------------------------------------------
main :: IO ()
main = do
  [arg] <- getArgs
  (ss, s) <- runs handler () workflow () arg
  writeFile "success-token.log" (unlines $
    [ "Input " ++ show arg ]
    ++ concatMap texts ss
    ++ [ "End of workflow." ])
  writeFile "success-token.dot" (graphviz workflow Nothing)


--------------------------------------------------------------------------------
workflow :: Workflow () String String String
workflow = Workflow "success" single [] [single]

single = Activity "SUCCESS"
  "Running SUCCESS..."
  (Pure $ \s k -> (s, "SUCCESS"))

handler name s k = error
  "This is a pure workflow; the handler shouldn't be called."
