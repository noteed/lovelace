module Main (main) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Functor.Identity
import System.Exit (exitFailure, exitSuccess)


import Lovelace
import qualified Identity
import qualified SuccessToken


-- Note: don't confuse the two Identity above: there the Identity monad to run
-- our workflows without IO, and there is the Identity workflow.



-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
-- | Helper to run pure workflows (pure handlers, no engine state, no object
-- state).
runPure handler workflow input =
  case stepResult (runIdentity (run handler () workflow () input)) of
    Token k -> k
    _ -> error "Workflow doesn't terminate with a token."


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps, scProps]

qcProps = testGroup "Checked by QuickCheck"
  [ QC.testProperty "identity x == x" $
      \x -> runPure Identity.handler Identity.workflow x == x
  , QC.testProperty "success x == \"SUCCESS\"" $
      \x -> runPure SuccessToken.handler SuccessToken.workflow x == "SUCCESS"
  ]

scProps = testGroup "Checked by SmallCheck"
  [ SC.testProperty "identity x == x" $
      \x -> runPure Identity.handler Identity.workflow x == x
  ]

unitTests = testGroup "Unit tests"
  [ testCase "identity hello == hello" $
      runPure Identity.handler Identity.workflow "hello"  @=? "hello"
  ]
