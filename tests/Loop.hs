{-# LANGUAGE OverloadedStrings #-}
module Loop where

import Data.Aeson (Object, Value(Number))
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc

import Lovelace hiding (final)
import StringWorkflows


-------------------------------------------------------------------------------
main :: IO ()
main = do
  s <- runs handler 3 workflow count0 "START"
  mapM_ print (fst s)
  print (snd s)


-------------------------------------------------------------------------------
workflow :: Workflow Object String String String
workflow = Workflow "example" initial transitions [final]

count0 = record [("count", int 0)]

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int :: Int -> Value
int = Number . fromIntegral

initial = Activity "START"
  "Initialization..."
  (Pure $ \state _ -> (state, "ASK_INPUT"))

second = Activity "INC"
  "Increase count..."
  (Pure $ \state _ ->
    let Number count = maybe (error "No count.") id $ H.lookup "count" state
        state' = record [("count", int (fromJust (Sc.toBoundedInteger count) + 1))]
    in (state', "ASK_INPUT"))

getInput = Activity "GET_INPUT"
  "Get input (`bye` to exit)..."
  (Task "ASK_INPUT")

final = Activity "FINAL"
  "End of workflow."
  (Pure $ \state _ ->
    let Number count = maybe (error "No count.") id $ H.lookup "count" state
    in (state, "SUCCESS"))

transitions = [
    (initial,  "ASK_INPUT", getInput),
    (getInput, "INC",       second),
    (getInput, "BYE",       final),
    (second,   "ASK_INPUT", getInput)
  ]

handler name s k =
  if s < 1
  then return (s, "BYE")
  else return (pred s, "INC")
