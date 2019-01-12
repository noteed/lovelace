{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
-- | Example for a single Pure activity.
module SuccessToken where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc

import Lovelace hiding (run)
import qualified Lovelace (run)
import StringWorkflows

-- | Run the example workflow.
main :: IO ()
main = do
  s <- Lovelace.run handler () workflow () "START"
  print s
  return ()

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

single = Activity "A"
  "Running A..."
  (Pure $ \s k -> (s, "SUCCESS"))

workflow :: Workflow () String String String
workflow = Workflow "pure" single [] [single]

handler name s k = error
  "This is a pure workflow; the handler shouldn't be called."
