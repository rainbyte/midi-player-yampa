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
data UICmd = LoadMidi !MidiFile.T | PlayPause | Stop | NoCmd
  deriving Show

type MidiEvent = Event MidiCmd
type UIEvent = Event UICmd

midiPlayer :: SF UIEvent MidiEvent
midiPlayer = initial

initial :: SF UIEvent MidiEvent
initial = switch (arr trigger) id
  where
    trigger :: UIEvent -> (MidiEvent, Event (SF UIEvent MidiEvent))
    trigger uiEv =
      case fromEvent uiEv of
        LoadMidi file -> (NoEvent, Event $ stopped file)
        _ -> (NoEvent, NoEvent)

playing :: MidiFile.T -> SF UIEvent MidiEvent
playing file = dkSwitch sf (arr trigger >>> notYet) (flip const)
  where
    sf = proc uiEv -> do
      midiEv <- midiStep file -< uiEv
      returnA -< midiEv -- Event $ MidiFile.Event.MetaEvent $ Meta.TrackName "lala.mid")
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ stopped newFile
        PlayPause -> Event $ paused file
        Stop -> Event $ stopped file
        _ -> NoEvent

midiStep :: MidiFile.T -> SF UIEvent (Event MidiCmd)
midiStep (MidiFile.Cons midiType _ tracks) =
  let midiEvents = EventList.toPairList $ MidiFile.mergeTracks midiType tracks
      fromETimePair = first (fromIntegral . MidiFile.fromElapsedTime)
      timedEvents = fmap fromETimePair midiEvents
  in afterEach timedEvents

stopped :: MidiFile.T -> SF UIEvent MidiEvent
stopped file = dSwitch (arr trigger >>> (notYet *** notYet)) id
  where
    trigger uiEv =
      case fromEvent uiEv of
        LoadMidi newFile -> (NoEvent, Event $ stopped newFile)
        PlayPause -> (NoEvent, Event $ playing file)
        _ -> (NoEvent, NoEvent)

paused :: MidiFile.T -> SF UIEvent MidiEvent
paused file = dkSwitch never (arr trigger >>> notYet) (flip const)
  where
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ stopped newFile
        PlayPause -> Event $ playing file
        Stop -> Event $ stopped file
        _ -> NoEvent
