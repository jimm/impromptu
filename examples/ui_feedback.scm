; http://impromptu.moso.com.au/extras/ui_feedback.mov

(define tr1
  (lambda (beat)
    (play synth1 48 40 .1)
    (play synth1 55 40 .1)
    (callback (*metro* (+ beat (* 1/2 1))) 'tr1 (+ beat 1))))

(tr1 (*metro* 'get-beat 8))

(define tr2
  (lambda (beat pitch)
    (play synth1 pitch (cosr 40 10 1) .1)
    (callback (*metro* (+ beat (* 1/2 1/3))) 'tr2 (+ beat 1/3)
              (if (member 0 '(60 62 67))
                  (random '(63 65 68))
                  (random '(60 62 67))))))

(tr2 (*metro* 'get-beat 8) 60)

(define tr3
  (lambda (beat ds)
    (play synth2 60 80 (* .2 (car ds)))
    (callback (*metro* (+ beat (* 1/2 (car ds)))) 'tr3 (+ beat (car ds))
              (if (null? (cdr ds))
                  (list 4 3 1)
                  (cdr ds)))))

(tr3 (*metro* 'get-beat 8) (list 4 3 1))
