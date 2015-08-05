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

    if argc != 3:
        print("Usage: python csv2midi.py <csv file> <output file name>")
        exit(1)

    # Pattern is the midi file. It can contain multiple tracks,
    # but we are using just one.
    pattern = midi.Pattern()
    track = midi.Track()
    pattern.append(track)
    
    print(argv[1])
    csvfile = open(argv[1], 'rb')
    counter = 0
    for row in csvfile:
        print(counter)
        vals = [x.strip() for x in row.split(',')]
        print(vals)
        vals = filter(None, vals)
        vals = map(lambda x: int(x), vals)
        tick = vals[0]
        base = vals[1] + 60
        print(len(vals))
        offsets = vals[2:]
        for i in range(0, len(offsets)):
            offsets[i] = int(offsets[i]) + base

        
        
        #print tick
        #print base
        #print offsets
        data = []
        
        track.append(midi.NoteOnEvent(tick=12, channel=1, data=[base] + [110]))
        for x in offsets:
            track.append(midi.NoteOnEvent(tick=0, channel=1, data=[x]+[110]))

        track.append(midi.NoteOffEvent(tick=tick, channel=1, data=[base] + [0]))
        for x in offsets:
            track.append(midi.NoteOffEvent(tick=0, channel=1, data=[x]+[0]))

        counter += 10


    # End of track appears 1 tick after last event
    track.append(midi.EndOfTrackEvent(tick=1))
    print("###########################################")
    print("ORIGINAL")
    print("###########################################")
    #pattern2 = midi.read_midifile("vexation.mid")
    #print(pattern2)
    #print("###########################################")
    #print("RECOVERED")
    #print("###########################################")
    #print pattern

    midi.write_midifile(argv[2], pattern)

if __name__ == "__main__": main(len(sys.argv), sys.argv)