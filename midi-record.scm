;;; This is the beginning of a simple sequencer. Maybe.
;;; Requires metronome.scm.

;; each element contains (time type a b)
;; caller must add device and output channel to this
(define *recording* ())
(define *old-io-midi-in* ())
(define *tempo* 120)                    ; metronome tempo
(define *metronome* (0, 10, 64))        ; device, channel, note

(define add-to-recording
  (lambda (t dev typ to-chan a b)
    (set! *recording* (cons (list t dev typ to-chan a b) *recording*))))

(define clean-up-recording
  (lambda ()
    (set! *recording* (calc-delta-times (reverse *recording*)))))

(define calc-delta-times
  (lambda (list)
    (do-calc-delta-times () 0 (car list) (cdr list))))

(define do-calc-delta-times
  (lambda (list prev-time event rest)
    (if (null? rest) (reverse list)
        (do-calc-delta-times (event-with-delta-time event prev-time)
                             (car event) (car rest) (cdr rest)))))

(define event-with-delta-time
  (lambda (event prev-time)
    (cons (if (= prev-time 0) 0 (- (car event) prev-time))
          (cdr event))))

(define midi-record
  (lambda (from to to-chan events dev typ chan a b)
    (when (or (null? from) (= dev from))
      (add-to-recording (now) dev typ to-chan a b))))

;; Start recording into *recording*.
(define midi-start-recording
  (lambda (from to to-chan)
    (set! *recording* '())
    (set! *old-io-midi-in* io:midi-in)
    (set! io:midi-in
          (lambda (dev typ chan a b)
            (midi-record from to to-chan dev typ chan a b)))
    (start-metronome
     *tempo*
     (lambda () (io:midi-out (now) *d4* *io:midi-on* 9 64 127)))))


;; Stop recording and clean up *recording*.
(define midi-stop-recording
  (lambda ()
    (set! io:midi-in *old-io-midi-in*)
    (stop-metronome)
    (clean-up-recording)))
