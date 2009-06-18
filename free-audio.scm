(define *free-alpha* (au:create-node "aumu" "AlpF" "LinP"))
(au:connect-node *free-alpha* 0 *au:output-node* 0)
(au:update-graph)

(au:print-graph)

(define *play* #t)

(define f
   (lambda (time)
      (play-note time *free-alpha* 48 80 5000)
      (play-note time *free-alpha* (random '(63 64)) 80 5000)
      (play-note time *free-alpha* (+ 0 67) 80 5000)
      (when *play*
          (callback (+ time 5000) f (+ time 5000)))))

(define play-f
   (lambda ()
      (set! *play* #t)
      (f (now))))

(play-f)
(define *play* #f)

(play-note (now) *free-alpha* 60 80 1000)

(au:open-view *free-alpha*)