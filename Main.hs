module Main where

import Sound.MIDI.File.Load
import System.Environment
import qualified Sound.MIDI.File as MFile
import qualified Sound.MIDI.File.Event as Event
import qualified Data.EventList.Relative.TimeBody as EventList

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
  EventList.mapBodyM print events
  return ()

-- printevts  

preent mf = (\(MFile.Cons tp div traks) -> show (length traks)) mf
