{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

module Sound.MidiPlayer
    ( UIEvent
    , UICmd(..)
    , MidiEvent
    , midiPlayer
    ) where

import           FRP.Yampa
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.PortMidi as MidiPM
import qualified Sound.PortMidi as PM

type MidiCmd = Either String PM.PMMsg
data PlayerState = Initial
                 | Playing MidiFile.T
                 | Stopped MidiFile.T
                 | Paused MidiFile.T
data UICmd = LoadMidi !MidiFile.T | PlayPause | Stop | NoCmd
  deriving Show

type MidiEvent = Event [MidiCmd]
type UIEvent = Event UICmd

midiPlayer :: SF UIEvent MidiEvent
midiPlayer = initial

initial :: SF UIEvent MidiEvent
initial = switch (arr trigger) nextSF
  where
    trigger uiEv = (NoEvent, transition Initial (fromEvent uiEv))
    nextSF f = f never

playing :: MidiFile.T -> SF UIEvent MidiEvent -> SF UIEvent MidiEvent
playing file sf = dkSwitch sf (arr trigger >>> notYet) nextSF
  where
    trigger (uiEv, _) = transition (Playing file) (fromEvent uiEv)
    nextSF oldSF f = f oldSF

midiStep :: MidiFile.T -> SF UIEvent MidiEvent
midiStep !midiFile =
  let pairs = MidiPM.fromMidiFileRelative midiFile
  in afterEachCat . fmap (first fromRational) $ pairs

stopped :: MidiFile.T -> SF UIEvent MidiEvent -> SF UIEvent MidiEvent
stopped file _ = switch (arr trigger >>> (notYet *** notYet)) nextSF
  where
    trigger uiEv = (NoEvent, transition (Stopped file) (fromEvent uiEv))
    nextSF f = f (midiStep file)

paused :: MidiFile.T -> SF UIEvent MidiEvent -> SF UIEvent MidiEvent
paused file cont = dkSwitch never (arr trigger >>> notYet) nextSF
  where
    trigger (uiEv, _) = transition (Paused file) (fromEvent uiEv)
    nextSF _ f = f cont

transition :: PlayerState
           -> UICmd
           -> Event (SF UIEvent MidiEvent -> SF UIEvent MidiEvent)
transition state uiCmd = case (state, uiCmd) of
  (_           , LoadMidi file) -> Event $ stopped file
  (Playing file, Stop)          -> Event $ stopped file
  (Paused file , Stop)          -> Event $ stopped file
  (Playing file, PlayPause)     -> Event $ paused file
  (Stopped file, PlayPause)     -> Event $ playing file
  (Paused file , PlayPause)     -> Event $ playing file
  _                             -> NoEvent
