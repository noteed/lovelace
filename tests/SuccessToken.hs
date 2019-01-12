-- | Example workflow with a single Pure activity.
module SuccessToken where

import Lovelace
import StringWorkflows


--------------------------------------------------------------------------------
main :: IO ()
main = run handler () workflow () "START" >>= print


--------------------------------------------------------------------------------
workflow :: Workflow () String String String
workflow = Workflow "pure" single [] [single]

single = Activity "A"
  "Running A..."
  (Pure $ \s k -> (s, "SUCCESS"))

handler name s k = error
  "This is a pure workflow; the handler shouldn't be called."
