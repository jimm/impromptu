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
