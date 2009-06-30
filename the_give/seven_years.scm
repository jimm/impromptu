(define sy-flow-1
  (list "Seven Years Flow 1"
        (list
           (list 'full-vol *kz* 0)
           (list 'pc *ws* 5 91)
           (list 'kz-pc 0 32))
        (mk-f only-from *mb*)
        (mk-f out *kz* 0)))

(play-flow sy-flow-1)
