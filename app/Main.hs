module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.IORef
import           Data.Time.Clock
import           FRP.Yampa
import           Sound.MidiPlayer
import qualified Sound.MIDI.File.Load as MidiFile.Load

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.Parser.Report as MidiParser

import qualified Sound.PortMidi as PM
import           System.Directory
import           System.Exit

initialize :: IO UIEvent
initialize = putStrLn "Initialize" >> pure (Event Stop)

outputActuate :: QSem -> PM.PMStream -> Bool -> MidiEvent -> IO Bool
outputActuate streamSem stream _ res =
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
        err <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
        signalQSem streamSem
        if err /= PM.NoError then do
          putStrLn $ "Error: " ++ show err
          pure True
        else
          pure False
      Left str -> do
        putStrLn ("Output: " ++ str)
        pure False

askMidiFile :: IO MidiFile.T
askMidiFile = askMidiFile' Nothing
  where
    askMidiFile' = fromMaybe $ do
      putStrLn "Enter midifile path:"
      path <- getLine
      putStrLn $ "Opening " ++ path
      exists <- doesFileExist path
      if exists then do
        file <- BL.readFile path
        let mMidifile = MidiFile.Load.maybeFromByteString file
        case MidiParser.result mMidifile of
          Left err -> do
            putStrLn $ "Error " ++ err
            putStrLn "Try again..."
            askMidiFile' Nothing
          Right midifile -> do
            putStrLn $ "Loading " ++ path
            pure midifile
      else do
        putStrLn $ "File " ++ path ++ " does not exist"
        putStrLn "Try again..."
        askMidiFile' Nothing

askCmd :: MVar UICmd -> IO ()
askCmd cmdVar = forever $ do
  putStrLn "\nEnter cmd:"
  cmd <- getLine
  uiCmd <- case cmd of
    "load" -> do
      midifile <- askMidiFile
      pure $ LoadMidi midifile
    "playPause" -> do
      putStrLn "Pressed Play/Pause button"
      pure PlayPause
    "stop" -> do
      putStrLn "Pressed Stop button"
      pure Stop
    _ ->
      pure NoCmd
  putMVar cmdVar uiCmd

main :: IO ()
main = do
  _ <- PM.initialize
  deviceCount <- PM.countDevices
  putStrLn "Output devices:"
  forM_ [0..deviceCount - 1] $ \deviceId -> do
    info <- PM.getDeviceInfo deviceId
    when (PM.output info) $
      putStrLn $ "  " ++ show deviceId ++ ". " ++ PM.name info
  putStr "Select: "
  selectedId <- readLn :: IO Int
  eStream <- PM.openOutput selectedId 0
  case eStream of
    Left stream -> mainYampa stream
    Right err -> do
      _ <- error $ show err
      exitFailure

mainYampa :: PM.PMStream -> IO ()
mainYampa stream = do
  cmdVar <- newEmptyMVar
  _ <- forkIO $ askCmd cmdVar
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
  reactimate initialize inputSense (outputActuate semStream stream) midiPlayer
  _ <- PM.close stream
  _ <- PM.terminate
  exitSuccess
