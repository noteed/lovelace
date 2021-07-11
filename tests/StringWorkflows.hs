-- | Orphan instances to make it easy to write example workflows.
{-# LANGUAGE FlexibleInstances #-} -- For instance Task String
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-} -- For instance Task String
module StringWorkflows where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sc

import Lovelace hiding (run)
import qualified Lovelace (run)

instance Task String where
  serializeTask = id

instance Token String String where
  tag = id
