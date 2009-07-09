(define lwim-flow-1
  (list "Look What I've Made Flow 1"
        (lambda ()
              (full-vol *px* 4)
              (full-vol *ws* 5)
              (pc *px* 4 23)
              (pc *ws* 5 35))
        (mk-f only-from *mb*)
        (mk-f out *px* 4)))

(play-flow lwim-flow-1)
