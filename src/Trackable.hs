{-# LANGUAGE OverloadedStrings #-}

module Trackable where

import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
-- import qualified System.IO as SIO
import Data.Monoid ((<>))
import qualified ShellUtil
import qualified Config as C
import qualified Handler as H
import qualified Handler.Git as HG
import qualified Reddup  as R
import Trackable.Data
import Trackable.Util
import Control.Monad.Reader

handleTrackable :: Trackable -> R.Reddup ()
handleTrackable trackable = do
  case trackable of
    (GitRepo grTrack) ->
      HG.gitHandler' grTrack
    (InboxDir idTrack) ->
      processInboxTrackable idTrack >>= H.handleInbox

processInboxTrackable :: InboxDirTrackable -> R.Reddup NHFile
processInboxTrackable idt@(InboxDirTrackable dir _locSpec)= do
  dir' <- lift $ pathToTextOrError dir
  R.verbose $ "checking " <>  dir'
  lift $ Tu.cd dir
  let files = lift $ Tu.ls dir
  files >>= (lift . return . NHFile idt)

configToTrackables :: R.Reddup Trackable
configToTrackables = do
  liftIO $ putStrLn "1"
  reddup <- ask
  liftIO $ putStrLn "2"
  location <- lift $ Tu.select $ C.locations $ C.rawConfig $ R.reddupConfig reddup
  liftIO $ putStrLn "3"
  lift $ locationSpecToTrackable location

locationSpecToTrackable :: C.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable ls = do
  liftIO $ putStrLn "4"
  let expand location =
        (Tu.fromText . Tu.lineToText) <$> (ShellUtil.expandGlob location)
  liftIO $ putStrLn "5"
  case ls of
    C.GitLoc location -> do
      liftIO $ putStrLn "6"
      path' <- (expand location)
      liftIO $ putStrLn "7"
      return $ GitRepo $ GitRepoTrackable path' ls
    C.InboxLoc location _foo -> do
      liftIO $ putStrLn "8"
      path' <- (expand location)
      liftIO $ putStrLn "9"
      return $ InboxDir $ InboxDirTrackable path' ls
