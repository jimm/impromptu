(define tp-progs
   (list
      (list 'pc *sj* 3 5)
      (list 'pc *ws* 5 81)
      (list 'pc *px* 4 23)
      (list 'kz-pc 0 109)))

(define tp-flow-1
   (list "Tommy's Place Flow 1"
         tp-progs
         (filter only-from *mb*)
         (filter multi
                 (list 
                        (list (filter range 0 55)
                              (filter xpose 36)
                              (filter out *sj* 3)
                              (filter xpose -12)
                              (filter out *px* 4))
                        (list (filter range 48 127)
                              (filter out *kz* 0))))))

(define tp-flow-2
   (list "Tommy's Place Flow 2"
         tp-progs
         (filter only-from *mb*)
         (filter multi
                 (list 
                    (list (filter range 0 55)
                          (filter xpose 36)
                          (filter out *sj* 3)
                          (filter out *ws* 5)
                          (filter xpose -12)
                          (filter out *px* 4))
                    (list (filter range 48 127)
                          (filter out *kz* 0))))))

(play-flow-list tp-flow-1 tp-flow-2)