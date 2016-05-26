{-# LANGUAGE Arrows #-}

module Sound.MidiPlayer
    ( UIEvent
    , UICmd(..)
    , MidiEvent
    , midiPlayer
    ) where

import           FRP.Yampa
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as MidiFile.Event

type MidiCmd = MidiFile.Event.T
data UICmd = LoadMidi MidiFile.T | PlayPause | Stop
  deriving Show

type MidiEvent = Event MidiCmd
type UIEvent = Event UICmd

midiPlayer :: SF UIEvent MidiEvent
midiPlayer = initial

initial :: SF UIEvent MidiEvent
initial = switch (arr trigger) stopped
  where
    trigger :: UIEvent -> (MidiEvent, Event MidiFile.T)
    trigger uiEv =
      let e = case fromEvent uiEv of
                LoadMidi file -> Event file
                _ -> NoEvent
      in (NoEvent, e)

playing :: MidiFile.T -> SF UIEvent (Event MidiCmd)
playing file = kSwitch (midiStep file) (arr trigger) (flip const)
  where
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ stopped newFile
        PlayPause -> Event $ paused file
        Stop -> Event $ stopped file

midiStep :: MidiFile.T -> SF UIEvent (Event MidiCmd)
midiStep (MidiFile.Cons midiType _ tracks) =
  let midiEvents = EventList.toPairList $ MidiFile.mergeTracks midiType tracks
      fromETimePair = first (fromIntegral . MidiFile.fromElapsedTime)
      timedEvents = fmap fromETimePair midiEvents
  in afterEach timedEvents

stopped :: MidiFile.T -> SF UIEvent MidiEvent
stopped file = dSwitch (arr trigger) id
  where
    trigger uiEv =
      case fromEvent uiEv of
        LoadMidi newFile -> (NoEvent, Event $ stopped newFile)
        PlayPause -> (NoEvent, Event $ playing file)
        _ -> (NoEvent, NoEvent)

paused :: MidiFile.T -> SF UIEvent MidiEvent
paused file = kSwitch never (arr trigger) (flip const)
  where
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ stopped newFile
        PlayPause -> Event $ playing file
        Stop -> Event $ stopped file
