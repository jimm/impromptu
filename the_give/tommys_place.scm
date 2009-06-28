(define tp-progs
   (list
      (list 'pc *sj* 3 5)
      (list 'pc *ws* 5 81)
      (list 'pc *px* 4 23)
      (list 'kz-pc 0 109)))

(define tp-flow-1
   (list "Tommy's Place Flow 1"
         tp-progs
         (mk-f only-from *mb*)
         (mk-f multi
                 (list 
                        (list (mk-f range 0 55)
                              (mk-f xpose 36)
                              (mk-f out *sj* 3)
                              (mk-f xpose -12)
                              (mk-f out *px* 4))
                        (list (mk-f range 48 127)
                              (mk-f out *kz* 0))))))

(define tp-flow-2
   (list "Tommy's Place Flow 2"
         tp-progs
         (mk-f only-from *mb*)
         (mk-f multi
                 (list 
                    (list (mk-f range 0 55)
                          (mk-f xpose 36)
                          (mk-f out *sj* 3)
                          (mk-f out *ws* 5)
                          (mk-f xpose -12)
                          (mk-f out *px* 4))
                    (list (mk-f range 48 127)
                          (mk-f out *kz* 0))))))

(play-flow-list tp-flow-1 tp-flow-2)
