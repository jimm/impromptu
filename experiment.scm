;;; ================================================================
;;; Pass throughs
;;; ================================================================

(define io:midi-in (pass-through *mb* *kz* 0))
(define io:midi-in (pass-through *mb* *tx1* 1))
(define io:midi-in (pass-through *mb* *tx2* 2))
(define io:midi-in (pass-through *mb* *sj* 3))
(define io:midi-in (pass-through *mb* *ws* 5))
(define io:midi-in (pass-through *mb* *px* 7))
(define io:midi-in (pass-through *mb* *d4* 9))

(stop-midi)

;;; ================================================================
;;; Test filters
;;; ================================================================
(define t1-pre
   (lambda ()
      (print "t1-pre")))

(define t2-pre
   (lambda ()
      (print "t2-pre")))

(define t1
   (lambda (arg dev typ chan a b)
      (print "t1 args =" arg dev typ chan a b)
      (list dev typ chan a b)))

(define t2
   (lambda (arg1 arg2 dev typ chan a b)
      (print "t2 args =" arg1 arg2 dev typ chan a b)
      (list dev typ chan a b)))

(define t3
   (lambda (dev typ chan a b)
     (print "t3 args =" dev typ chan a b)
     (list dev typ chan a b)))

(define test-xpose
   (lambda (amt dev typ chan a b)
      (list dev typ chan (if (is-note-type typ) (+ a amt) a) b)))

(define let-nothing-through
   (lambda (arg dev typ chan a b)
      ()))

;;; ================================================================
;;; Example flows
;;; ================================================================

(define test-filter-list
   (list
         (filter t1 "my-arg-1")
         (filter t2 "my-arg-1" "my-arg-2")
         (filter t3)
         (filter multi
                 (list (filter t1 "multi t1")
                       (filter t2 "multi t2 arg 1" "multi t2 arg 2")))
         (filter let-nothing-through)
         (filter t1 "my-arg-1-again")))

(define test-pre-list '((t1-pre) (t2-pre)))

(define test-filter-list
   (list (filter t1 "this is the filter") (filter t2 "second in filter chain" "aha")))

(define test-flow
   (append (list test-pre-list) test-filter-list))

(append '((a b)) test-filter-list)

(print test-filter-list)
(print test-flow)
(print (car test-flow))
(print (cdr test-flow))

(play-flow test-flow)

(debug "================")
(play-filters test-filter-list (list "dev" "typ" "chan" "a" "b"))
(multi '((t1 "multi t1")
         (t2 "multi t2 arg 1" "multi t2 arg 2"))
       "dev" "typ" "chan" "a" "b")

(filter out *sj* 3)
(macro-expand (filter out *sj* 3))

(play-flow
  (list
    ; pre list
    (('kz-pc 0 2))
    ; remaining elements are filters
    ('out *sj* 3)))


(list '((kz-pc 0 2)) (list 'out *sj* 3))

(io:print-midi-destinations)
(stop-midi)
(set! io:midi-out ())

;;; ================================================================
;;; test code
;;; ================================================================

;; play one note on sj using raw io:midi-out
(begin
  (io:midi-out (now) *sj* *io:midi-on* 3 60 64)
  (io:midi-out (+ *second* (now)) *sj* *io:midi-off* 3 60 64)
)

(kz-pc 1 103)
(kz-pc 1 3)

; play one note on sj using "out". first is note on, second is note off
(out *sj* 3 *mb* *io:midi-on* 1 60 64)
(out *sj* 3 *mb* *io:midi-off* 1 60 64)

; This is how to run a function in a constant list. You need to eval the car
; (the function name).
(define x '(some-func arg1 arg2 arg3))
(apply (eval (car x)) (cdr x))

; Example, done explicitly
(define my-flow
  (lambda ()
    (kz-pc 0 3)
    (set! io:midi-in
      (lambda (dev type chan a b)
        (if (= dev *mb*)                ; from mb
            (if (is-note-type type)
                (if (and (>= a 60) (<= a 80)) ; range 60 - 80
                    (io:midi-out (now) *kz* type chan (+ a 12) b))  ; xpose
                ; non-note passes through
                (io:midi-out (now) *kz* type chan a b)))))))
(my-flow)

; Simple mb -> kz
(set! io:midi-in
   (lambda (dev typ chan a b)
      (if (= dev *mb*)
          (io:midi-out (now) *kz* typ 0 a b))))

(stop-midi)
