(define metronome-on #f)

(define metronome
   (lambda (time tempo func)
      (when metronome-on
            ; do something at time
            (func)
            (let ((t (+ time (/ (* 60 *second*) tempo))))
               (callback t metronome t tempo func)))))

(define start-metronome
   (lambda (tempo func)
      (set! metronome-on #t)
      (metronome (now) tempo func)))

(define stop-metronome
   (lambda ()
      (set! metronome-on #f)
      (print "metronome stopped")))

(start-metronome 120 (lambda () (print "tick...")))
(stop-metronome)

; tests of funcs in time-lib.scm

(define foo (make-metro 120))
(print (foo 2.0))
(print (foo 'get-time '(1))) ; ?
(print (foo 'get-beat '(1 2 3))) ; ?
(print (foo 'get-tempo))
(foo 'set-tempo 120) ; ?
(print (foo 'dur 1.5))
(foo '(get-time . 2))

(define bar (make-timeline '((0 . 'start) (1 . 'one) (2 . 'two))))
(print (bar 1.5))
(print (bar 0))
(print (bar 0.5))
