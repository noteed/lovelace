{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-} -- For instance Token K G
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
module Main (main) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Data.List (mapAccumL, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc
import System.Random

import Lovelace

-- | The main function is an engine for our particular workflow type, i.e. it
-- handles/synchronizes multiple workflow instances, and processes specific
-- tasks (e.g. register a Shoot and deliver a Hit later).
main :: IO ()
main = do
  let marines = map (start workflow)
	[ record [("name", String "Kurt")]
        , record [("name", String "Sigourney")]
        ]
  g <- newStdGen
  loop (initialState g) marines

  where

  loop s marines = do
    putStrLn $ "Tick " ++ show (eTick s)
    mapM_ (\m -> do let name = getName $ getState m
                        life = getLife $ getState m
                    putStrLn $ show name ++ " " ++ show life
          ) marines
    if all (> 0) $ map (getLife . getState) marines
      then do
        let (s1, marines1) = mapAccumL runThink s marines
        let (s2, marines2) = mapAccumL runHit s1 marines1
        loop s2 { eTick = eTick s2 + 1 } marines2
      else putStrLn "Game complete."

  runThink s (Step w a r _) =
    let (r1, gen) = random (eRandom s)
        st@(Step _ _ _ t) = continue w a r (THINK_ r1)
    in runTask s { eRandom = gen } st t

  runHit s st@(Step _ _ r _) =
    let p (ticks, target, _) = ticks == eTick s && target == getName r
        f (Step w a' r' _) (_, _, damage) = continue w a' r' (HIT_ damage)
        (hits, rest) = span p $ eShoots s
        st' = foldl f st hits
    in (s { eShoots = rest }, st')

  runTask s st t = case t of
    Task (Shoot ticks target damage) ->
      (s { eShoots = reorder $ (eTick s + ticks, target, damage) : eShoots s }, st)
    Task Wait -> (s, st)
    Token k -> error "This particular workflow never generates tokens directly."

getState (Step _ _ r _) = r

getLife state = life
  where
  Number life = maybe (error "No life attribute.") id $ H.lookup "life" state

getName state = name
  where
  String name = maybe (error "No name attribute.") id $ H.lookup "name" state

reorder = sortBy f
  where f (aTick, aName, _) (bTick, bName, _) = compare (aTick, aName) (bTick, bName)

----------------------------------------------------------------------
-- Example workflow.
----------------------------------------------------------------------

data Engine = Engine
  { eShoots :: [(Int, Text, Int)] -- ticks, target, damage - a.k.a schedule a Hit.
  , eTick :: Int -- current tick
  , eRandom :: StdGen
  }

initialState = Engine [] 0

data T =
    Shoot Int Text Int -- ticks, target, damage - a.k.a schedule a Hit.
  | Wait -- Block the workflow until the engine decides the next step.
  deriving Show

data K =
    THINK_ Int -- random number
  | HIT_ Int -- damage
  | END_
  deriving Show

data G =
    THINK
  | HIT
  | END
  deriving (Eq, Show)

instance Task T where
  serializeTask t = case t of
    Shoot ticks target damage -> "shoot " ++ show damage ++ " at " ++ show target ++ " at " ++ show ticks
    Wait -> "wait"

instance Token K G where
  tag (THINK_ _) = THINK
  tag (HIT_ _) = HIT
  tag END_ = END

-- | Similar to `object`, but don't turn it in a `Value`.
record :: [Pair] -> Object
record = H.fromList

int :: Int -> Value
int = Number . fromIntegral

initial = Activity "INIT"
  "Initializing marine..." $ \state _ ->
  (H.insert "life" (int 100) state, Task Wait)

think = Activity "THINK"
  "Thinking..." $ \state (THINK_ random_) ->
  let life = getLife state
      name = getName state
      gen = mkStdGen random_
      random1 :: Int
      (random1, gen') = randomR (1, 100) gen
      (random2, gen'') = randomR (1, 26) gen'
      (random3, _) = randomR (15, 35) gen''
  in if name == "Sigourney"
     then let target = "Kurt"
              doesShoot = random1 > 25
              damage = random2
          in if doesShoot
             then (state, Task $ Shoot 3 target damage)
             else (state, Task Wait)
     else let target = "Sigourney"
              doesShoot = random1 > 50
              damage = random3
          in if doesShoot
             then (state, Task $ Shoot 4 target damage)
             else (state, Task Wait)

damage = Activity "DAMAGE"
  -- Yes, this is a partial function...
  "Getting damage..." $ \state (HIT_ damage) ->
  let life = getLife state
  in (H.insert "life" (int $ max 0 (fromJust (Sc.toBoundedInteger life) - damage)) state, Task Wait)

final = Activity "FINAL"
  "Party is over..." $ \state _ ->
  (state, Task Wait) -- Dummy task.

transitions = [
    ((initial, THINK), think),
    ((think, THINK), think),
    ((think, HIT), damage),
    ((think, END), final),
    ((damage, THINK), think),
    ((damage, END), final)
  ]

workflow :: Workflow T K G
workflow = Workflow "marines" initial transitions [final]
