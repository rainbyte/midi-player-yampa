module Main where

import           Control.Monad
import           Data.IORef
import           Data.Time.Clock
import           FRP.Yampa
import           Sound.MidiPlayer
import qualified Sound.MIDI.File.Event as MidiFile.Event
import qualified Sound.MIDI.File.Load as MidiFile.Load

initialize :: IO UIEvent
initialize = putStrLn "Initialize" >> pure (Event Stop)

outputActuate :: Bool -> Event MidiFile.Event.T -> IO Bool
outputActuate changed ev = do
  when changed $
    putStrLn ("sendMidiEvent: " ++ show ev)
  pure False

askCmd :: IO UICmd
askCmd = do
  putStrLn "\nEnter cmd:"
  cmd <- getLine
  case cmd of
    "load" -> do
      file <- MidiFile.Load.fromFile "/tmp/Flourish.mid"
      pure $ LoadMidi file
    "playPause" -> pure PlayPause
    "stop" -> pure Stop
    s -> do
      putStrLn ("Received wrong cmd: " ++ s)
      askCmd

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  let inputSense :: Bool -> IO (DTime, Maybe UIEvent)
      inputSense _ = do
        now' <- getCurrentTime
        lastTime <- readIORef timeRef
        writeIORef timeRef now'
        let dt = now' `diffUTCTime` lastTime
        cmd <- askCmd
        pure (realToFrac dt, Just $ Event cmd)
  reactimate initialize inputSense outputActuate midiPlayer
