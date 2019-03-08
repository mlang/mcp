{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Radio (Player, stations, Command(..), Forked(..)) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.GI.Base.Properties (getObjectPropertyString, setObjectPropertyInt64, setObjectPropertyString)
import Data.Maybe
import Data.String
import Data.Text (pack, unpack)
import qualified GI.GLib as GLib
import qualified GI.Gst as Gst
import System.Console.ANSI

type Player = Command -> IO ()
type Printer = String -> IO ()

stations :: Printer -> IO Player
stations eprint = do
  void $ Gst.init Nothing
  glibLoop <- GLib.mainLoopNew Nothing True
  liftIO . forkIO $ #run glibLoop
  playBin <- fromJust <$> Gst.elementFactoryMake "playbin" (Just "Player")
  bus <- fromJust <$> #getBus playBin
  watchId <- #addWatch bus GLib.PRIORITY_DEFAULT (busCall eprint playBin)
  radio <- atomically newTChan
  forkIO $ background playBin radio
  return . frontend $ radio

frontend :: TChan Command -> Player
frontend backend = atomically . writeTChan backend

background :: Gst.Element -> TChan Command -> IO ()
background playBin radio = do
  let continue = background playBin radio
  cmd <- atomically $ readTChan radio
  case cmd of
    SetURL uri -> do
      void $ #setState playBin Gst.StateNull
      setObjectPropertyString playBin "uri" (Just (pack uri))
      void $ #setState playBin Gst.StatePlaying
      continue

    Quit -> do
      putStrLn "Radio quit"
      pure ()

    _ -> continue

data Command = Quit | SetURL String

class Forked a where
  quit :: a

instance Forked Command where
  quit = Quit

instance IsString Command where
  fromString = SetURL

busCall :: Printer -> Gst.Element -> Gst.Bus -> Gst.Message -> IO Bool
busCall externalPrint playBin bus message = do
  messageTypes <- Gst.getMessageType message
  when (Gst.MessageTypeEos `elem` messageTypes) $ do
    externalPrint "End of stream"
  when (Gst.MessageTypeError `elem` messageTypes) $ do
    (gerror,_debug) <- #parseError message
    errorMsg <- Gst.gerrorMessage gerror
    externalPrint $ "Error: " <> unpack errorMsg
  when (Gst.MessageTypeBuffering `elem` messageTypes) $ do
    percent <- #parseBuffering message
    void $ #setState playBin if percent < 100 then Gst.StatePaused else Gst.StatePlaying
  when (Gst.MessageTypeTag `elem` messageTypes) $ do
    tagList <- #parseTag message
    #foreach tagList $ \tl t -> case t of
      "title" -> do
        (ok, title) <- #getString tl t
        when ok $ externalPrint (setSGRCode [SetItalicized True] <>
                                 "Now playing: " <> unpack title <>
                                 setSGRCode [SetItalicized False])
      _ -> pure ()
  pure True
