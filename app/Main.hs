{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu

import qualified Config as C
import qualified Trackable as Track
import qualified Options as O
import qualified Reddup  as R
import Control.Monad.Reader

extractConfig :: Either String C.Config -> Tu.Shell C.Config
extractConfig eitherConfig =
  let
    die :: String -> Tu.Shell C.Config
    die errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either die return eitherConfig

checkConfig' :: C.Config -> Tu.Shell C.ProcessedConfig
checkConfig' config =
  let
    checkResult = C.processConfig config
    die :: Tu.Text -> Tu.Shell C.ProcessedConfig
    die = Tu.die  . ("error in config: " <>)
  in either (die . C.configErrorsDisplay) return checkResult

main :: IO ()
main = Tu.sh $ do
  opts <- O.parseOpts
  O.debug opts $ T.pack $ show opts
  eitherConfig <- C.loadConfig
  configUnchecked <- extractConfig eitherConfig
  pconfig <- checkConfig' configUnchecked
  O.debug opts $ T.pack $ show pconfig
  let reddup = R.Reddup pconfig opts
  runReaderT doIt reddup

doIt :: ReaderT R.Reddup Tu.Shell ()
doIt = do
  reddup <- ask
  let pconfig = R.reddupConfig reddup
  let opts    = R.reddupOptions reddup
  let trackables = Track.configToTrackables pconfig
  Track.handleTrackables trackables
  lift $ O.debug opts "done"
