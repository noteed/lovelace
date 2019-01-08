module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import Lovelace

import Identity hiding (main)


-------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Dummy."
  s <- Lovelace.run handler () workflow () "Hello"
  case stepResult s of
    Token "Hello" -> do
      putStrLn "SUCCESS."
      exitSuccess
    _ -> do
      putStrLn "FAILURE."
      exitFailure
