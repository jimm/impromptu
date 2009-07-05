(define yb-flow-1
  (list "Yes, But Flow 1"
        (list
              (list 'pc *sj* 3 7)
              (list 'kz-pc 0 2)
              (list 'pc *ws* 5 18))
        (mk-f only-from *mb*)
        (mk-f multi
              (list
                 (list
                    (mk-f range 74 127)
                    (mk-f xpose -12)
                    (mk-f out *sj* 3))
                 (list
                    (mk-f range 0 73)
                    (mk-f xpose 12)
                    (mk-f out *kz* 0))))))

(play-flow yb-flow-1)
