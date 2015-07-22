module Main where

import Sound.MIDI.File.Load
import System.Environment
import qualified Sound.MIDI.File as MFile
import qualified Sound.MIDI.File.Event as Event
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Sound.MIDI.Message as MMessage

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as Voice



main = do
  args <- getArgs
  if (length args /= 1)
    then do
      putStrLn "syntax:"
      putStrLn "midi2csv <filename>"
    else do
      loadIt (args !! 0)

loadIt fp = do 
  -- showFile fp
  (MFile.Cons tp div tracks) <- fromFile fp
  -- print event count.
  let events = MFile.mergeTracks tp tracks
  print $ (length tracks)
  print (EventList.duration events)
  -- EventList.mapBodyM print events
  -- mapM print (EventList.toPairList events)
  
  mapM (\(a,b) -> print (toNote b)) (EventList.toPairList events)
  return ()

toNote evt = 
  case evt of 
    Event.MIDIEvent blah -> 
      let wha = ChannelMsg.messageBody blah in 
      case wha of 
        ChannelMsg.Voice (Voice.NoteOn p v) -> Just (Voice.fromPitch p)
        _ -> Nothing
      -- Just wha
    _ -> Nothing
      
{-

toNoteClusters :: [(time, body)] 

toNoteClusters (ev:evs) = 
  foldrPair blah 

-}

-- printevts  
preent mf = (\(MFile.Cons tp div traks) -> show (length traks)) mf
