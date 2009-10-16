;;; see also print-available.scm

;; Generates a list of all TYPE nodes of the form
;;   (("Name" type subtype manuf) ("Name" type subtype manuf)...)
(define generate-node-list
  (lambda (type)
    (map (lambda (str)
           (cons (cadr (pregexp-match "(.*?)\\.\\.\\.+" str))
                 (list (cadr (pregexp-match "type='(....)'" str))
                       (cadr (pregexp-match "subtype='(....)'" str))
                       (cadr (pregexp-match "manufacturer='(....)'" str)))))
         (cdr (pregexp-split "\n" (au:get-node-strings type))))))

(define node-list-names (lambda (list) (map car list)))

(define *aumu-node-list* (generate-node-list "aumu"))
(define *aufx-node-list* (generate-node-list "aufx"))

; (print *aumu-node-list*)
; (print (node-list-names *aumu-list*))
; (assoc "FreeAlpha" *aumu-list*)

; Example: (my-make-node "FreeAlpha" *aumu-node-list*)
(define my-make-node
  (lambda (name node-list)
    (let ((n (assoc name node-list)))
      (au:make-node (cadr n) (caddr n) (cadddr n)))))

;(define x (my-make-node "FreeAlpha" *aumu-node-list*))
;(au:open-view x)
;(au:clear-graph)
;(au:connect-node x 0 *au:output-node* 0)
;(au:update-graph)
;(begin
;  (play-note (now) x 80 127 *second*)
;  (play-note (now) x 84 127 *second*)
;)
