(define sy-flow-1
  (list "Seven Years Flow 1"
        (flow-setup
           (full-vol *kz* 0)
           (pc *ws* 5 91)
           (kz-pc 0 32))
        (mk-f only-from *mb*)
        (mk-f out *kz* 0)))

(play-flow sy-flow-1)
