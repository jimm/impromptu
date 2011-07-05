;; My setup.

(require "utils")

(begin

  ;; MIDI destinations

  (define *ws* (io:midi-destination 1))  ; Wavestation, chan 6
  (define *kz* (io:midi-destination 2))  ; Kurzweil K2000, chan 1,7-9,11-16
  (define *px* (io:midi-destination 3))  ; Kurzweil PX1000, chan 5
  (define *sj* (io:midi-destination 4))  ; Super Jupiter, chan 4
  (define *tx1* (io:midi-destination 5)) ; TX-81Z #1, chan 2
  (define *tx2* (io:midi-destination 5)) ; TX-81Z #2, chan 3
  (define *d4* (io:midi-destination 6))  ; D4, chan 10

  (define *mb* (io:midi-source 0))       ; Midiboard
  (define *ws-in* (io:midi-source 1))    ; Wavestation

  ;; Instrument assoc lists with name, src, dest, chan.

  (define (make-dest name dest chan)
    (list (cons "name" name) (cons "dest" dest) (cons "chan" chan)))

  (define *i:mb* '(("name" . "Midiboard") ("src" . *mb*)))
  (define *i:ws* (cons '("src" . *ws-in*) (make-dest "Wavestation" *ws* 5)))
  (define *i:kz1* (make-dest "Kurzweil K2000 chan 1" *kz* 0))
  (define *i:kz7* (make-dest "Kurzweil K2000 chan 7" *kz* 6))
  (define *i:kz8* (make-dest "Kurzweil K2000 chan 8" *kz* 7))
  (define *i:kz9* (make-dest "Kurzweil K2000 chan 9" *kz* 8))
  (define *i:kz11* (make-dest "Kurzweil K2000 chan 11" *kz* 10))
  (define *i:kz12* (make-dest "Kurzweil K2000 chan 12" *kz* 11))
  (define *i:kz13* (make-dest "Kurzweil K2000 chan 13" *kz* 12))
  (define *i:kz14* (make-dest "Kurzweil K2000 chan 14" *kz* 13))
  (define *i:kz15* (make-dest "Kurzweil K2000 chan 15" *kz* 14))
  (define *i:kz16* (make-dest "Kurzweil K2000 chan 16" *kz* 15))
  (define *i:px* (make-dest "Kurzweil PX1000" *px* 4))
  (define *i:sj* (make-dest "Super Jupiter" *sj* 3))
  (define *i:tx1* (make-dest "TX-81Z #1" *tx1* 1))
  (define *i:tx2* (make-dest "TX-81Z #2" *tx2* 2))
  (define *i:d4* (make-dest "D4" *d4* 9))

  ;; Functions to access assoc list name, src, dest, chan.

  (attr-reader i name)
  (attr-reader i src)
  (attr-reader i dest)
  (attr-reader i chan)
)

(define *ss* (io:midi-destination 1))    ; SimpleSynth virtual input
