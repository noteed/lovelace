{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
-- | Example for a build/run of Docker images based on GitHub hooks.
module Main (main) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc

import Lovelace hiding (run)
import qualified Lovelace

-- | Run the example workflow.
main :: IO ()
main = do
  s <- Lovelace.run runTask () workflow initialRecord "START"
  -- print s
  return ()

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

instance Task String where
  serializeTask = id

instance Token String String where
  tag = id

data Record = Record
  deriving Show

initialRecord = Record

record = Record -- TODO Update the record

build = Activity "BUILD"
  "Building Docker image..."
  (Task "RUN-DOCKER-BUILD")

run = Activity "RUN"
  "Running Docker image..."
  (Task "RUN-DOCKER-RUN")

success = Activity "SUCCESS"
  "End of workflow."
  (Pure $ \state _ -> (state, "STOP"))

failure = Activity "FAILURE"
  "End of workflow."
  (Pure $ \state _ -> (state, "IGNORED"))

transitions = [
    ((build, "IMAGE_ID_XXX"), run),
    ((build, "FAILURE"), failure),
    ((run, "ARTIFACT_ID_XXX"), success),
    ((run, "FAILURE"), failure)
  ]

workflow :: Workflow Record String String String
workflow = Workflow "build-and-run" build transitions [success, failure]

runTask name s k = do
  putStrLn $ "Running task " ++ name ++ "..."
  case name of
    "RUN-DOCKER-BUILD" -> do
      line <- getLine
      if line == "failure"
        then return (s, "FAILURE")
        else return (s, "IMAGE_ID_XXX")
    "RUN-DOCKER-RUN" -> do
      line <- getLine
      if line == "failure"
        then return (s, "FAILURE")
        else return (s, "ARTIFACT_ID_XXX")
    _ -> return (s, "FAILURE")
