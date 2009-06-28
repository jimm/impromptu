(define lwim-flow-1
  (list "Look What I've Made Flow 1"
        (list
              (list 'pc *ws* 5 35)
              (list 'pc *px* 4 23))
        (mk-f only-from *mb*)
        (mk-f out *px* 4)))

(play-flow lwim-flow-1)
