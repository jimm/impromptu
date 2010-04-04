;; An attempt to build a song as Scheme code.
;(load-my-file "utils")
;(string->note "c5")

;; ====

(define-macro (times n . body)
  `(dotimes (,(gensym) ,n) ,@body))

(define-macro (times-cons n l val)
  `(times ,n (set! ,l (cons ,val ,l))))

;; ====

(define intro-rh
  (let ((l ()))
    (times-cons (* 12 8) l '(74 67))
    l))

(define intro-lh
  (let ((l ()))
    (times 3
      (times-cons 12 l '(73 66))
      (times-cons 12 l '(71 64)))
    (times-cons 12 l '(73 66))
    (times-cons 6 l '(71 64))
    (times-cons 3 l '(66 59))
    (times-cons 3 l '(64 57))
    (reverse l)))

(define make-intro
  (lambda (rh lh l)
    (if (null? rh) (reverse l)
       (make-intro (cdr rh) (cdr lh) (cons (car rh) (cons (car lh) l))))))

(define intro (make-intro intro-rh intro-lh ()))

;; ====

(define rh-pat '((86 81) 74 (85 81) 74 81 74))

(define rh-meas
  (let ((l ()))
    (times-cons 4 l rh-pat l)
    l))
