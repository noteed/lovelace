{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
module Main (main) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as H

import Lovelace

-- | Run the example workflow.
main :: IO ()
main = do
  s <- run runTask () workflow (record [])
  print s
  print $ serialize s

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

instance Task String where
  serializeTask = id

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int = Number . I

initial = Activity "INIT"
  "Initialization..." $
  const (record [("count", int 0)], Token "SECOND")

second = Activity "SECOND"
  "Get input (`bye` to exit)..." $ \state ->
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
      state' = record [("count", int (count + 1))]
  in (state', Task "ASK_INPUT")

third = Activity "THIRD"
  "Will wait for task completion..." $ \state ->
  (state, Task "TASK")

final = Activity "FINAL"
  "End of workflow." $ \state ->
  let Number (I count) = maybe (error "No count.") id $ H.lookup "count" state
  in (state, Token "STOP")

transitions = [
    ((initial, "SECOND"), second),
    ((second, "SECOND"), second),
    ((second, "BYE"), third),
    ((third, "FINAL"), final)
  ]

workflow :: Workflow String String
workflow = Workflow "example" initial transitions [final]

runTask s name = do
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
