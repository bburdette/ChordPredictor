# ChordPredictor
Analyse and generate chord sequences

ChordPredictor is a set of software tools to generate randomized midi files containing chord sequences.

midi2csv is a haskell program that converts midi files to CSV format, while 'canonicalizing' chords.  It only processes groups of 3 or more simultaneous notes. 

the scala/figaro component reads in the CSV files and builds probability tables for duration, chord type, and root note.  It uses these to generate randomized midi files.

csv2midi is a python program that reads in the csv files from scala and creates midi files for playback.  

### Building midi2csv:

- first install haskell and cabal, the haskell build tool.  If your cabal is new enough, hopefully the following will work:

```
> cd midi2csv
> cabal sandbox init  (optional step, but recommended)
> cabal install
```

The executable will end up in midi2csv/.cabal_sandbox/bin/midi2csv.  If you don't use the sandbox, then it will go in ~/.cabal/bin.  

### Using midi2csv

You can use midi2csv with no parameters to get the syntax.  Essentially you just pass it the filename of the file you want to convert, and the filename of the output file.  For example:

```
> midi2csv vexation.mid vexation.csv
```
### Using the scala component

In order to run the model as is, you only need to change the names in 
the src/main/scala/fileNames.scala file.  Direct them as follows:

 - melodyMatrixFileName= the file in which you will store the note transitions
 - testFileDirectory= the directory in which the training csv files are held.
 - newSongFileName= the name of the file in which you will store the new song
 - chordMatrixFileName= the file in which you store the chord transitions
 - timeMatrixFileName= the name of the file in which you will store the time transitions.
 
Change the length of the song in figModel if you want (variable is numIterations) it is currently at 100.

Then run musicProcessing.figModel as your main file.  That is it.

### Using csv2midi
csv2midi is a Python 2 program that converts output CSV file to playable midis.
It requires the Python package Midi. To install:

```
> pip install midi
```

To run csv2midi, run csv2midi.py through the Python 2 interpreter:

```
> python csv2midi <input CSV file> <output MIDI file>
```

Example:

```
> python csv2midi free_bird.csv free_bird.mid
```
Then use the midi player of your choice to listen to free_bird.mid!
