;;; ================================================================
;;; Pass throughs
;;; ================================================================

(define io:midi-in (pass-through *mb* *kz* 0))
(define io:midi-in (pass-through *mb* *tx1* 1))
(define io:midi-in (pass-through *mb* *tx2* 2))
(define io:midi-in (pass-through *mb* *sj* 3))
(define io:midi-in (pass-through *mb* *ws* 5))
(define io:midi-in (pass-through *mb* *px* 4))
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
   (lambda (arg dev type chan a b)
      (print "t1 args =" arg dev type chan a b)
      (list dev type chan a b)))

(define t2
   (lambda (arg1 arg2 dev type chan a b)
      (print "t2 args =" arg1 arg2 dev type chan a b)
      (list dev type chan a b)))

(define t3
   (lambda (dev type chan a b)
     (print "t3 args =" dev type chan a b)
     (list dev type chan a b)))

(define test-xpose
   (lambda (amt dev type chan a b)
      (list dev type chan (if (note-type? type) (+ a amt) a) b)))

(define let-nothing-through
   (lambda (arg dev type chan a b)
      ()))

;;; ================================================================
;;; Test flows
;;; ================================================================

(define test-filter-list
   (list
         (mk-f t1 "my-arg-1")
         (mk-f t2 "my-arg-1" "my-arg-2")
         (mk-f t3)
         (mk-f multi
                 (list (mk-f t1 "multi t1")
                       (mk-f t2 "multi t2 arg 1" "multi t2 arg 2")))
         (mk-f let-nothing-through)
         (mk-f t1 "my-arg-1-again")))

(define test-pre-proc (lambda () (t1-pre) (t2-pre)))


(define test-filter-list
   (list (mk-f t1 "this is the filter")
         (mk-f t2 "second in filter chain" "aha")))

(define other-test-filter-list
   (list (mk-f t1 "SECOND FLOW ONLY FILTER")))

(define test-flow
   (append test-pre-proc test-filter-list))

(define other-test-flow
   (append (lambda () (t1-pre)) other-test-filter-list))

(append '((a b)) test-filter-list)

(print test-filter-list)
(print test-flow)
(print other-test-flow)
(print (car test-flow))
(print (cdr test-flow))

(play-flow test-flow)

(play-flow-list test-flow other-test-flow)

(debug "================")
(play-filters test-filter-list (list "dev" "type" "chan" "a" "b"))
(multi '((t1 "multi t1")
         (t2 "multi t2 arg 1" "multi t2 arg 2"))
       "dev" "type" "chan" "a" "b")

(mk-f out *sj* 3)
(macro-expand (mk-f out *sj* 3))

(play-flow
  (list
    ; pre proc
    (lambda () (kz-pc 0 2))
    ; remaining elements are filters
    (mk-f out *sj* 3)))

(io:print-midi-destinations)
(stop-midi)
(set! io:midi-out ())

;;; ================================================================
;;; Example flows
;;; ================================================================

(play-flow (list "Midiboard to K2000" () (mk-f out *kz* 0)))
(play-flow (list "Midiboard to Wavestation" () (mk-f out *ws* 5)))
(play-flow (list "Midiboard to PX" () (mk-f out *px* 4)))
(play-flow (list "Midiboard to Super Jupiter" () (mk-f out *sj* 3)))
(play-flow (list "Midiboard to TX 1" () (mk-f out *tx1* 1)))
(play-flow (list "Midiboard to TX 2" () (mk-f out *tx2* 2)))
(play-flow (list "Midiboard to D4" () (mk-f out *d4* 9)))

(define only-from-mb (mk-f only-from *mb*))

(play-flow-list
   (list "Midiboard to K2000" () only-from-mb (mk-f out *kz* 0))
   (list "Midiboard to Wavestation" () only-from-mb (mk-f out *ws* 5))
   (list "Midiboard to PX" () only-from-mb (mk-f out *px* 4))
   (list "Midiboard to Super Jupiter" () only-from-mb (mk-f out *sj* 3))
   (list "Midiboard to TX 1" () only-from-mb (mk-f out *tx1* 1))
   (list "Midiboard to TX 2" () only-from-mb (mk-f out *tx2* 2))
   (list "Midiboard to D4" () only-from-mb (mk-f out *d4* 9))
)

(play-flow (list "multi test" ()
                 (mk-f multi
                         (list
                               (list (mk-f out *sj* 3)
                                     (mk-f out *d4* 9))
                               (list (mk-f out *ws* 5))))))

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
