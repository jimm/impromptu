(begin

  (define *ws* (io:midi-destination 1))  ; Wavestation
  (define *kz* (io:midi-destination 2))  ; Kurzweil K2000
  (define *px* (io:midi-destination 3))  ; Kurzweil PX1000
  (define *sj* (io:midi-destination 4))  ; Super Jupiter
  (define *tx1* (io:midi-destination 5)) ; TX-81Z #1, chan 2
  (define *tx2* (io:midi-destination 5)) ; TX-81Z #2, chan 3
  (define *d4* (io:midi-destination 6))  ; D4

  (define *mb* (io:midi-source 0))       ; Midiboard
  (define *ws-in* (io:midi-source 1))    ; Wavestation

)

(define *ss* (io:midi-destination 1))    ; SimpleSynth virtual input
