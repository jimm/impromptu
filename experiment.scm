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
      (list dev typ chan (if (note-type? typ) (+ a amt) a) b)))

(define let-nothing-through
   (lambda (arg dev typ chan a b)
      ()))

;;; ================================================================
;;; Test flows
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
   (list (filter t1 "this is the filter")
         (filter t2 "second in filter chain" "aha")))

(define other-test-filter-list
   (list (filter t1 "SECOND FLOW ONLY FILTER")))

(define test-flow
   (append (list test-pre-list) test-filter-list))

(define other-test-flow
   (append (list '((t1-pre))) other-test-filter-list))

(append '((a b)) test-filter-list)

(print test-filter-list)
(print test-flow)
(print other-test-flow)
(print (car test-flow))
(print (cdr test-flow))

(play-flow test-flow)

(play-flow-list test-flow other-test-flow)

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

(io:print-midi-destinations)
(stop-midi)
(set! io:midi-out ())

;;; ================================================================
;;; Example flows
;;; ================================================================

(play-flow (list "Midiboard to K2000" () (filter out *kz* 0)))
(play-flow (list "Midiboard to Wavestation" () (filter out *ws* 5)))
(play-flow (list "Midiboard to PX" () (filter out *px* 4)))
(play-flow (list "Midiboard to Super Jupiter" () (filter out *sj* 3)))
(play-flow (list "Midiboard to TX 1" () (filter out *tx1* 1)))
(play-flow (list "Midiboard to TX 2" () (filter out *tx2* 2)))
(play-flow (list "Midiboard to D4" () (filter out *d4* 9)))

(define only-from-mb (filter only-from *mb*))

(play-flow-list
   (list "Midiboard to K2000" () only-from-mb (filter out *kz* 0))
   (list "Midiboard to Wavestation" () only-from-mb (filter out *ws* 5))
   (list "Midiboard to PX" () only-from-mb (filter out *px* 4))
   (list "Midiboard to Super Jupiter" () only-from-mb (filter out *sj* 3))
   (list "Midiboard to TX 1" () only-from-mb (filter out *tx1* 1))
   (list "Midiboard to TX 2" () only-from-mb (filter out *tx2* 2))
   (list "Midiboard to D4" () only-from-mb (filter out *d4* 9))
)

(play-flow (list "multi test" ()
                 (filter multi
                         (list
                               (list (filter out *sj* 3)
                                     (filter out *d4* 9))
                               (list (filter out *ws* 5))))))

;;; ================================================================
;;; test code
;;; ================================================================

(gfx:start-live-video)

(define loopy
   (lambda (time)
      (gfx:draw-image time *flow-list-canvas* (gfx:get-live-frame) 0.5)
      (callback (+ time 2000) 'loopy (+ time 3000))))
(loopy (now))
(define loopy ())
