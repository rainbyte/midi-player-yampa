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

data PlayerCmd = PlayerOutput [MidiCmd] Bool
type PlayerEvent = Event PlayerCmd

midiPlayer :: SF UIEvent MidiEvent
midiPlayer = initial >>> arr (mapFilterE fromPlayerEvent)
  where
    fromPlayerEvent x = case x of
      PlayerOutput midiCmds _ -> Just midiCmds

initial :: SF UIEvent PlayerEvent
initial = switch (arr trigger) nextSF
  where
    trigger uiEv = (NoEvent, transition Initial (fromEvent uiEv))
    nextSF f = f never

playing :: MidiFile.T -> SF UIEvent PlayerEvent -> SF UIEvent PlayerEvent
playing file sf = dkSwitch sf (arr trigger >>> notYet) nextSF
  where
    trigger (_, Event (PlayerOutput _ True)) = transition (Playing file) Stop
    trigger (uiEv, _) = transition (Playing file) (fromEvent uiEv)
    nextSF oldSF f = f oldSF

midiStep :: MidiFile.T -> SF UIEvent PlayerEvent
midiStep !midiFile =
  let pairs = MidiPM.fromMidiFileRelative midiFile
  in proc _ -> do
    midiMsgs <- afterEachCat . fmap (first fromRational) $ pairs -< ()
    remainingMsgs <- accumBy (\n l -> n - length l) (length pairs) -< midiMsgs
    let stop = event False (<=0) remainingMsgs
    returnA -< event NoEvent (\x -> Event $ PlayerOutput x stop) midiMsgs

stopped :: MidiFile.T -> SF UIEvent PlayerEvent -> SF UIEvent PlayerEvent
stopped file _ = switch (arr trigger >>> (notYet *** notYet)) nextSF
  where
    trigger uiEv = (NoEvent, transition (Stopped file) (fromEvent uiEv))
    nextSF f = f (midiStep file)

paused :: MidiFile.T -> SF UIEvent PlayerEvent -> SF UIEvent PlayerEvent
paused file cont = dkSwitch never (arr trigger >>> notYet) nextSF
  where
    trigger (uiEv, _) = transition (Paused file) (fromEvent uiEv)
    nextSF _ f = f cont

transition :: PlayerState
           -> UICmd
           -> Event (SF UIEvent PlayerEvent -> SF UIEvent PlayerEvent)
transition state uiCmd = case (state, uiCmd) of
  (_           , LoadMidi file) -> Event $ stopped file
  (Playing file, Stop)          -> Event $ stopped file
  (Paused file , Stop)          -> Event $ stopped file
  (Playing file, PlayPause)     -> Event $ paused file
  (Stopped file, PlayPause)     -> Event $ playing file
  (Paused file , PlayPause)     -> Event $ playing file
  _                             -> NoEvent
