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
import Data.List
import qualified Data.Set as S
import qualified Numeric.NonNegative.Class as NN

main = do
  args <- getArgs
  let largs = length args
  if (largs == 1)
    then 
      loadIt (args !! 0)
    else if (largs == 2)
      then 
        showFile (args !! 1)
      else do
        putStrLn "syntax:"
        putStrLn "midi2csv <filename>"
        putStrLn "midi2csv -raw <filename>"

loadIt fp = do 
  -- showFile fp
  (MFile.Cons tp div tracks) <- fromFile fp
  -- print event count.
  let events = MFile.mergeTracks tp tracks
  -- print $ (length tracks)
  -- print (EventList.duration events)
  -- EventList.mapBodyM print events
  -- mapM print (EventList.toPairList events)
  -- mapM (\(a,b) -> print $ show a ++ " " ++ show (toNote b)) (EventList.toPairList events)
  let ncs = toNoteClusters (EventList.toPairList events)
      -- chords = filter (\(t,cl) -> length cl > 2) ncs
      chords = backOne $ addLiminate 3 0 ncs
      -- chords = addLiminate 3 0 ncs
  mapM (\(t,cl) -> do 
    putStr (show t)
    mapM (\c -> do
      putStr ","
      putStr (show c)) (toRootAndIntervals cl)
    putStrLn "") chords
  -- mapM print $ filter (\(t,cl) -> length cl > 2) ncs
  -- mapM print $ filter (\(_,lst) -> length lst > 2) (toNoteClusters (EventList.toPairList events))
  return ()

-- add up the note start offsets for eliminated notes and store the accumulated time in the next chord.
addLiminate :: (NN.C time, Num time, Eq time) => Int -> time -> [(time, [Int])] -> [(time, [Int])]
addLiminate lowerbound accum ((curtime,curnotes):rest) = 
  if length curnotes < lowerbound 
    then addLiminate lowerbound (accum + curtime) rest
    else (curtime + accum, curnotes) : addLiminate lowerbound 0 rest
addLiminate _ accum [] = [(accum,[])]

backOne :: [(time, [Int])] -> [(time, [Int])]
backOne ((a,b):(c,d):moar) = (c,b) : backOne ((c,d):moar)
backOne [(a,b)] = []
backOne [] = []
 

toNote :: Event.T -> Maybe Int
toNote evt = 
  case evt of 
    Event.MIDIEvent blah -> 
      let wha = ChannelMsg.messageBody blah in 
      case wha of 
        ChannelMsg.Voice (Voice.NoteOn p v) -> Just (Voice.fromPitch p)
        _ -> Nothing
    _ -> Nothing
    
toRootAndIntervals :: [Int] -> [Int]
toRootAndIntervals (x:xs) = 
  let intervals = S.fromList $ filter (/= 0) $ map (\y -> rem (y - x) 12) xs in
  x:(S.toList intervals) 
toRootAndIntervals [] = []
 
toNoteClusters :: (Num time, Eq time) => [(time, Event.T)] -> [(time,[Int])]
toNoteClusters inlist = 
  let tmnotes = dropWhile (\(_,mb) -> mb == Nothing) $ map feh inlist
      feh (tm,bdy) = (tm, toNote bdy) 
    in
      case tmnotes of 
        [] -> []
        ((tm,Just nt):rest) -> 
          let ncs = tnc rest (tm,[nt]) in
            map (\(t,n) -> (t, sort n)) ncs

tnc :: (Num time, Eq time) => [(time, Maybe Int)] -> (time, [Int]) -> [(time,[Int])]
tnc ((newtime,mbnote):rest) (curtime,curnotes) = 
  if (newtime == 0) 
    then case mbnote of
      (Just newnote) -> tnc rest (curtime,newnote:curnotes)
      Nothing -> tnc rest (curtime,curnotes)
    else case mbnote of
      (Just newnote) -> (curtime,curnotes) : tnc rest (newtime, [newnote]) 
      Nothing -> (curtime,curnotes) : tnc rest (newtime, [])
tnc [] (curtime,curnotes) = [(curtime,curnotes)]

{-

fold versions:  foldr and foldl turn out quite different!  both are wrong.

toNoteClusters :: (Num time, Eq time) => [(time, Event.T)] -> [(time,[Int])]
toNoteClusters inlist = 
  let tmnotes = dropWhile (\(_,mb) -> mb == Nothing) $ map feh inlist
      feh (tm,bdy) = (tm, toNote bdy) 
    in
      case tmnotes of 
        [] -> []
        ((tm,Just nt):pairs) -> 
          foldl (\((t,notes):tnlist) (time, mbn) -> 
            if (time == 0)
              then case mbn of 
                -- for time zero, we're stacking notes onto the existing time entry.
                (Just note) -> (t, note:notes):tnlist
                Nothing -> (t, notes):tnlist
              else case mbn of
                -- if new time, start new time entry list. 
                (Just note) -> (time, [note]):(t,notes):tnlist
                Nothing -> (time, []):(t,notes):tnlist
                )
            [(tm,[nt])] pairs

toNoteClusters :: (Num time, Eq time) => [(time, Event.T)] -> [(time,[Int])]
toNoteClusters inlist = 
  let tmnotes = dropWhile (\(_,mb) -> mb == Nothing) $ map feh inlist
      feh (tm,bdy) = (tm, toNote bdy) 
    in
      case tmnotes of 
        [] -> []
        ((tm,Just nt):pairs) -> 
          foldr (\(time, mbn) ((t,notes):tnlist) -> 
            if (time == 0)
              then case mbn of 
                -- for time zero, we're stacking notes onto the existing time entry.
                (Just note) -> (t, note:notes):tnlist
                Nothing -> (t, notes):tnlist
              else case mbn of
                -- if new time, start new time entry list. 
                (Just note) -> (time, [note]):(t,notes):tnlist
                Nothing -> (time, []):(t,notes):tnlist
                )
            [(tm,[nt])] pairs
-}

{-
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
