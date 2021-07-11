{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
module Main (main) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc
import System.Environment (getArgs)

import Lovelace hiding (final)

-- | Run the example workflow.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["graph"] -> writeFile "example.dot" (graphviz workflow Nothing)
    _ -> do
      (ss, _) <- runs handler () workflow (record [("count", int 0)]) "START"
      putStrLn "\nTrace of resulting tokens:"
      mapM_ (print . stepResult) ss
      when (args == ["--graph"]) $
        mapM_ (\(i, s) -> do
          putStrLn ("Writing file example " ++ show i ++ ".dot...")
          writeFile
            ("example-" ++ show i ++ ".dot")
            (graphvizs s)) (zip [1..] ss)

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

instance Task String where
  serializeTask = id

instance Token String String where
  tag = id

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int :: Int -> Value
int = Number . fromIntegral

initial = Activity "INIT"
  "Initialization..."
  (Pure $ \state _ -> (state, "SECOND"))

second = Activity "SECOND"
  "Increase count..." (Pure $ \state _ ->
  let Number count = maybe (error "No count.") id $ H.lookup "count" state
      state' = record [("count", int (fromJust (Sc.toBoundedInteger count) + 1))]
  in (state', "ASK_INPUT"))

getInput = Activity "GET_INPUT"
  "Get input (`bye` to exit)..."
  (Task "ASK_INPUT")

third = Activity "THIRD"
  "Will wait for task completion..."
  (Task "TASK")

final = Activity "FINAL"
  "End of workflow." (Pure $ \state _ ->
  let Number count = maybe (error "No count.") id $ H.lookup "count" state
  in (state, "SUCCESS"))

transitions = [
    (initial,  "SECOND",    second),
    (second,   "ASK_INPUT", getInput),
    (getInput, "SECOND",    second),
    (getInput, "BYE",       third),
    (third,    "FINAL",     final)
  ]

workflow :: Workflow Object String String String
workflow = Workflow "example" initial transitions [final]

handler name s k = do
  putStrLn $ "Running task " ++ name ++ "..."
  if name == "ASK_INPUT"
    then do
      line <- getLine
      if line == "bye"
        then return (s, "BYE")
        else do
          putStrLn line
          return (s, "SECOND")
    else return (s, "FINAL")
