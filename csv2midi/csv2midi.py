# Henry Cooney
# hacoo36@gmail.com
#
# Chord Predictor -- https://github.com/bburdette/ChordPredictor
# csv2midi.py
# 27 July 2015
#
# Converts formatted CSV files to midi. Used in the Chord Predictor,
# built for 2015 PPAML Summer School.

import csv
import midi
import sys

def main(argc, argv):

    if argc != 2:
        print("Usage: python csv2midi.py <csv file>")
        exit(1)

    # Pattern is the midi file. It can contain multiple tracks,
    # but we are using just one.
    pattern = midi.Pattern()
    track = midi.Track()
    pattern.append(track)
    
    print(argv[1])
    csvfile = open(argv[1], 'rb')
    for row in csvfile:
        vals = [x.strip() for x in row.split(',')]
        vals = map(lambda x: int(x), vals)
        print vals
        tick = vals[0]
        base = vals[1]
        offsets = vals[2:]
        for i in range(0, len(offsets)):
            offsets[i] = int(offsets[i]) + base

        
        
        print tick
        print base
        print offsets
        data = []
        
        track.append(midi.NoteOnEvent(tick=10, channel=1, data=[base] + [110]))
        for x in offsets:
            track.append(midi.NoteOnEvent(tick=0, channel=1, data=[x]+[110]))

        track.append(midi.NoteOffEvent(tick=tick, channel=1, data=[base] + [0]))
        for x in offsets:
            track.append(midi.NoteOffEvent(tick=0, channel=1, data=[x]+[0]))



    # End of track appears 1 tick after last event
    track.append(midi.EndOfTrackEvent(tick=1))
    print("###########################################")
    print("ORIGINAL")
    print("###########################################")
    pattern2 = midi.read_midifile("vexation.mid")
    print(pattern2)
    print("###########################################")
    print("RECOVERED")
    print("###########################################")
    print pattern

    midi.write_midifile("output.mid", pattern)

if __name__ == "__main__": main(len(sys.argv), sys.argv)