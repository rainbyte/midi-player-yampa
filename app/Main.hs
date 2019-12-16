{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import           Data.Time.Clock
import           FRP.Yampa
import qualified Graphics.UI.Webviewhs as WHS
import           Language.Javascript.JMacro
import           Sound.MidiPlayer
import qualified Sound.MIDI.File.Load as MidiFile.Load

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.Parser.Report as MidiParser

import qualified Sound.PortMidi as PM
import           System.Directory
import           System.Exit

initialize :: IO UIEvent
initialize = putStrLn "Initialize engine" >> pure (Event Stop)

outputActuate :: QSem -> MVar Int -> [PM.PMStream] -> Bool -> MidiEvent -> IO Bool
outputActuate streamSem streamVar streams _ res =
  case res of
    Event msgs ->
      foldM handleMsg False msgs
    NoEvent ->
      pure False
  where
    handleMsg True _ = pure True -- stop reactimate
    handleMsg False eMsg = case eMsg of
      Right msg -> do
        waitQSem streamSem
        streamId <- readMVar streamVar
        let stream = streams !! streamId
        err <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
        signalQSem streamSem
        case err of
          Right _    -> pure False
          Left pmerr -> do
            putStrLn $ "Error: " ++ show pmerr
            pure True
      Left str -> do
        putStrLn ("Output: " ++ str)
        pure False

midiFromPath :: String -> IO (Maybe MidiFile.T)
midiFromPath path = do
  putStrLn $ "Opening " ++ path
  exists <- doesFileExist path
  if exists then do
    file <- BL.readFile path
    let mMidifile = MidiFile.Load.maybeFromByteString file
    case MidiParser.result mMidifile of
      Left err -> do
        putStrLn $ "Error " ++ err
        putStrLn "Try again..."
        pure Nothing
      Right midifile -> do
        putStrLn $ "Loading " ++ path
        pure $ Just midifile
  else do
    putStrLn $ "File " ++ path ++ " does not exist"
    putStrLn "Try again..."
    pure Nothing

htmlGUI :: MVar UICmd -> MVar Int -> [(String, PM.PMStream)] -> IO ()
htmlGUI cmdVar streamVar streams = void $ do
  dir <- getCurrentDirectory
  WHS.withWindowLoop
    WHS.WindowParams
      { WHS.windowParamsTitle = "midi-player-hs"
      , WHS.windowParamsUri = T.pack $ "file://" ++ dir ++ "/app/Main.html"
      , WHS.windowParamsWidth = 600
      , WHS.windowParamsHeight = 340
      , WHS.windowParamsResizable = False
      , WHS.windowParamsDebuggable = True
      }
    handleJSRequest
    (WHS.WithWindowLoopSetUp    (\ _window -> do
      putStrLn "Initialize GUI"
      -- Add output ports to combobox
      forM_ (zip ([1..]::[Int]) streams) $ \(idx, (name,_)) ->
        WHS.runJavaScript _window [jmacro| portAdd(`idx`, `name`); |]))
    (WHS.WithWindowLoopTearDown (void . pure . const))
    windowCallback
 where
  handleJSRequest window request = case request of
    "load"      -> WHS.withWindowOpenDialog
      window
      "Open MIDI file"
      False
      openMidiFile
    "playpause" -> putMVar cmdVar PlayPause
    "stop"      -> putMVar cmdVar Stop
    (T.stripPrefix "port" -> Just suf) ->
      case TR.decimal suf of
        Right (idx, _) -> do
          _ <- swapMVar streamVar idx
          TIO.putStrLn $ "Selected port" <> suf
        _ -> pure ()
    _ -> pure ()
  windowCallback _ = pure True
  openMidiFile filename = do
    mMidifile <- midiFromPath $ T.unpack filename
    case mMidifile of
      Just midifile -> putMVar cmdVar (LoadMidi midifile)
      Nothing -> pure ()

main :: IO ()
main = do
  _ <- PM.initialize
  deviceCount <- PM.countDevices
  putStrLn "Output devices:"
  streams <- fmap catMaybes $ forM [0..deviceCount - 1] $ \deviceId -> do
    info <- PM.getDeviceInfo deviceId
    when (PM.output info) $
      putStrLn $ "  " ++ show deviceId ++ ". " ++ PM.name info
    eStream <- PM.openOutput deviceId 0
    pure $ either (const Nothing) (\stream -> pure (PM.name info, stream)) eStream
  case streams of
    _:_ -> do
      cmdVar <- newEmptyMVar
      streamVar <- newMVar 0
      _ <- forkIO $ mainYampa cmdVar streamVar (fmap snd streams)
      htmlGUI cmdVar streamVar streams
      forM_ streams (PM.close . snd)
      _ <- PM.terminate
      exitSuccess
    [] -> do
      _ <- error "Output device not available"
      exitFailure

mainYampa :: MVar UICmd -> MVar Int -> [PM.PMStream] -> IO ()
mainYampa cmdVar stream streams = do
  t <- getCurrentTime
  timeRef <- newIORef t
  let inputSense :: Bool -> IO (DTime, Maybe UIEvent)
      inputSense _ = do
        now' <- getCurrentTime
        lastTime <- readIORef timeRef
        writeIORef timeRef now'
        let dt = now' `diffUTCTime` lastTime
        mCmd <- tryTakeMVar cmdVar
        let cmd = fromMaybe NoCmd mCmd
        pure (realToFrac dt, Just $ Event cmd)
  semStream <- newQSem 1
  reactimate initialize inputSense (outputActuate semStream stream streams) midiPlayer
  pure ()
