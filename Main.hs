module Main where

import Sound.MIDI.File.Load
import System.Environment
import qualified Sound.MIDI.File as MFile
import qualified Sound.MIDI.File.Event as Event
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Sound.MIDI.Message as MMessage

import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import Data.Maybe


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
  
  mapM (\(a,b) -> print $ show a ++ show (toNote b)) (EventList.toPairList events)
  print (toNoteClusters (EventList.toPairList events))
  return ()

toNote :: Event.T -> Maybe Int
toNote evt = 
  case evt of 
    Event.MIDIEvent blah -> 
      let wha = ChannelMsg.messageBody blah in 
      case wha of 
        ChannelMsg.Voice (Voice.NoteOn p v) -> Just (Voice.fromPitch p)
        _ -> Nothing
    _ -> Nothing
     
toNoteClusters :: (Num time, Eq time) => [(time, Event.T)] -> [(time,[Int])]
toNoteClusters inlist = 
  let tmnotes = catMaybes $ map feh inlist
      feh (tm,bdy) = case (toNote bdy) of 
        Just note -> Just (tm, note)
        Nothing -> Nothing
   in
      case tmnotes of 
        [] -> []
        ((tm,nt):pairs) -> 
          foldr (\(time, n) ((t,notes):tnlist) -> 
              if (time == 0)
                then ((t, n:notes):tnlist)
                else ((tm, [n]):(t,notes):tnlist))
            [(tm,[nt])] pairs


{-
 
toNoteClusters :: [(time, body)] -> [(time,[Int])]
toNoteClusters inlist = 
  let rest = dropWhile (\(_,x) -> toNote x == Nothing) inlist in
  case rest of 
    [] -> []
    ((tm,bdy):pairs) -> 
      foldr (\(time, body) ((t,notes):tnlist) -> 
          let note = toNote body in
          case (time, note) of 
            (0, Just n) -> ((t, n:notes):tnlist)
            (tm, Just n) -> ((tm, [n]):(t,notes):tnlist)
            (0, Nothing) -> ((t,notes):tnlist)
            (tm, Nothing) -> ((tm, []):(t,notes):tnlist))
        [(tm,toNote bdy)] pairs

--      foldr (\(time, body) ((t,notes):tnlist)) 
-}

-- printevts  
preent mf = (\(MFile.Cons tp div traks) -> show (length traks)) mf
