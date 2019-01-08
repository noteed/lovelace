{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
-- | The identity workflow. This is also an example of a single Pure activity.
module Identity where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc
import System.Environment (getArgs)

import Lovelace hiding (run)
import qualified Lovelace (run)


-------------------------------------------------------------------------------
-- | Run the example workflow.
main :: IO ()
main = do
  [arg] <- getArgs
  s <- Lovelace.run handler () workflow () arg
  print s
  return ()

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

instance Task String where
  serializeTask = id

instance Token String String where
  tag = id

single = Activity "DONE"
  "Running DONE..."
  (Pure $ \s k -> (s, k))

workflow :: Workflow () String String String
workflow = Workflow "pure" single [] [single]

handler name s k = error
  "This is a pure workflow; the handler shouldn't be called."
