;;; This file is not to be executed as a whole.

; This isn't one for-each so I can easily re-load any one of them.
(load-my-file "utils")
(load-my-file "midi-consts")
(load-my-file "metronome")
(load-my-file "midi-record")

(io:print-midi-sources)
(io:print-midi-destinations)
(io:print-midi-devices)

;; Make sure MidiPipe has a keyboard going to its own MidiPipe Output output 1,
;; and SimpleSynth is listing to its own SimpleSynth virtual input.
(define src (io:midi-source 1))       ; MidiPipe Output 1
(define dest (io:midi-destination 1)) ; SimpleSynth virtual input
(define drums dest)                   ; SimpleSynth virtual input

;; metronome test
;; *tempo* default value is 120, defined in midi-record.
(start-midi-metronome-std drums 120)
(stop-midi-metronome)

;; passthrough
(set! io:midi-in (lambda (dev type chan a b) (io:midi-out (now) dest type chan a b)))

;; test bogus metronome
(define *metronome* ())
(ensure-metronome-defined)
(midi-start-recording src dest 0)

;; test recording from anywhere
(define *metronome* (list drums *gm-drum-channel* *gm-closed-hi-hat*))
(midi-start-recording () dest 0)
(define track (midi-stop-recording))
(print track)

(define tracks ())
(define track ())

;; test recording from a specific device
(define *metronome* (list drums *gm-drum-channel* *gm-closed-hi-hat*))
(midi-start-recording src dest 0 tracks)
(set! track (midi-stop-recording))
(set! tracks (cons track tracks))       ; Save track into tracks
(print "track =" track)
(print "tracks =" tracks)

;; test playback of just-recorded track
(play-track track)

;; test playback of all tracks
(play-tracks tracks)
(begin
  (start-midi-metronome-std drums 120)
  (play-track (car tracks))
)
(stop-midi-metronome)
(stop-playing)
(set! tracks (cdr tracks)) ; remove latest track

(print "reversed recording =" (reverse *recording*))
(print "to delta =" (calc-delta-times (reverse *recording*)))

; Test calc-delta-times
(define test-list '((42 0 1 2) (44 2 3 4) (45 1 2 3) (52 7 8 9)))
(print (calc-delta-times test-list))
