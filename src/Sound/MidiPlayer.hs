{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

module Sound.MidiPlayer
    ( UIEvent
    , UICmd(..)
    , MidiEvent
    , midiPlayer
    ) where

import           Data.Maybe
import           FRP.Yampa
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.PortMidi as MidiPM
import qualified Sound.PortMidi as PM

type MidiCmd = Either String PM.PMMsg
data UICmd = LoadMidi !MidiFile.T | PlayPause | Stop | NoCmd
  deriving Show

type MidiEvent = Event [MidiCmd]
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

playing :: MidiFile.T -> Maybe (SF UIEvent MidiEvent) -> SF UIEvent MidiEvent
playing file mCont =
  dkSwitch (fromMaybe sf mCont) (arr trigger >>> notYet) nextSF
  where
    sf = proc uiEv -> do
      midiEv <- midiStep file -< uiEv
      returnA -< midiEv
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ const (stopped newFile)
        PlayPause -> Event $ paused file
        Stop -> Event $ const (stopped file)
        _ -> NoEvent
    nextSF cont f = f cont

midiStep :: MidiFile.T -> SF UIEvent MidiEvent
midiStep !midiFile =
  let pairs = MidiPM.fromMidiFileRelative midiFile
  in afterEachCat . fmap (first fromRational) $ pairs

stopped :: MidiFile.T -> SF UIEvent MidiEvent
stopped file = switch (arr trigger >>> (notYet *** notYet)) id
  where
    trigger uiEv =
      case fromEvent uiEv of
        LoadMidi newFile -> (NoEvent, Event $ stopped newFile)
        PlayPause -> (NoEvent, Event $ playing file Nothing)
        _ -> (NoEvent, NoEvent)

paused :: MidiFile.T -> SF UIEvent MidiEvent -> SF UIEvent MidiEvent
paused file cont = dkSwitch never (arr trigger >>> notYet) (flip const)
  where
    trigger (uiEv, _) =
      case fromEvent uiEv of
        LoadMidi newFile -> Event $ stopped newFile
        PlayPause -> Event $ playing file (Just cont)
        Stop -> Event $ stopped file
        _ -> NoEvent
