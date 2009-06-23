;;; Graphics noodling

(define *flow-list-canvas* (gfx:make-canvas 400 600))
(define *flow-list-text-style* (gfx:make-text-style "Times-Roman" 16.0 (list 0 0 0 1)))
(define *fl-color-curr* (list 0.8 0.8 0.8 1))
(define *fl-color* (list 0.5 0.5 0.5 1))

(define draw-nth-flow
   (lambda (canvas n curr-n flow-list)
      (let ((bot (- 600 (* (+ n 1) 20))))
         (gfx:draw-path (now) canvas
                        (gfx:make-rectangle 0 bot 400 20)
                        (list 0 0 0 1)
                        (if (= n curr-n) *fl-color-curr* *fl-color*)
                        (list 0 1 0 1))
         (gfx:draw-text (now) canvas
                        (string-append (number->string (+ n 1))
                                       " "
                                       (flow-name (list-ref flow-list n)))
                        *flow-list-text-style* (list 8 bot 400 20)))))

(define draw-flow-list
   (lambda (canvas n flow-list)
      (when canvas
         (gfx:clear-canvas (now) canvas (list 0.25 0.25 0.25 1))
         (dotimes (i (length flow-list))
            (draw-nth-flow canvas i n flow-list)))))

; (draw-flow-list *flow-list-canvas* 0 '(a b c))

(define close-flow-list-canvas (lambda () (gfx:close-canvas *flow-list-canvas*)))
