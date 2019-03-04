-- | The identity workflow. This is also an example of a single Pure activity.
module Identity where

import System.Environment (getArgs)

import Lovelace
import StringWorkflows


-------------------------------------------------------------------------------
main :: IO ()
main = do
  [arg] <- getArgs
  (ss, s) <- runs handler () workflow () arg
  writeFile "identity.log" (unlines $
    [ "Input " ++ show arg ]
    ++ concatMap texts ss
    ++ [ "End of workflow." ])
  writeFile "identity.dot" (graphviz workflow Nothing)


-------------------------------------------------------------------------------
workflow :: Workflow () String String String
workflow = Workflow "identity" single [] [single]

single = Activity "DONE"
  "Running DONE..."
  (Pure $ \s k -> (s, k))

handler name s k = error
  "This is a pure workflow; the handler shouldn't be called."
