;; Examples:
;;   (start-metronome 120 (lambda () (print "tick...")))
;;   (stop-metronome)

(define metronome-on #f)

(define metronome
  (lambda (time tempo func)
    (when metronome-on
          (func)                        ; do something at time
          (let ((t (+ time (/ (* 60 *second*) tempo))))
            (callback t metronome t tempo func)))))

(define start-metronome
  (lambda (tempo func)
    (set! metronome-on #t)
    (metronome (now) tempo func)))

(define stop-metronome
  (lambda ()
    (set! metronome-on #f)))

;; Uses the metronome to play clicks using my D4. Assumes both
;; midi-setup.scm and midi-consts.scm have been loaded.
(define start-clicks
  (lambda (tempo)
    (start-metronome
     tempo
     (lambda () (io:midi-out (now) *d4* *gm-drum-channel*
                             *gm-closed-hi-hat* 127)))))

(define stop-clicks stop-metronome)
