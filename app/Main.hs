module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Data.IORef
import           Data.Time.Clock
import           FRP.Yampa
import           Sound.MidiPlayer
import qualified Sound.MIDI.File.Event as MidiFile.Event
import qualified Sound.MIDI.File.Load as MidiFile.Load

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import Data.EventList.Relative.MixedBody ((/.), (./), )
import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Sound.PortMidi as PM
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

askCmd :: MVar UICmd -> IO ()
askCmd cmdVar = forever $ do
  putStrLn "\nEnter cmd:"
  cmd <- getLine
  uiCmd <- case cmd of
    "load" -> do
      putStrLn "Enter midifile path:"
      path <- getLine
      midifile <- case path of
        "default" ->
          pure midiExample
        _ -> do
          putStrLn "Opening /tmp/Flourish.mid"
          file <- MidiFile.Load.fromFile "/tmp/Flourish.mid"
          putStrLn "Loading /tmp/Flourish.mid"
          pure file
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

midiExample :: MidiFile.T
midiExample =
   let chan = ChannelMsg.toChannel 3
       vel  = VoiceMsg.toVelocity 64
   in  MidiFile.Cons MidiFile.Parallel (MidiFile.Ticks 10)
          [0 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 20) vel))) ./
           4 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 24) vel))) ./
           4 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOn (VoiceMsg.toPitch 27) vel))) ./
           7 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 20) vel))) ./
           4 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 24) vel))) ./
           4 /. MidiFile.Event.MIDIEvent (ChannelMsg.Cons chan (ChannelMsg.Voice (VoiceMsg.NoteOff (VoiceMsg.toPitch 27) vel))) ./
           EventList.empty]

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
