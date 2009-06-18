(begin
  (print "Audio Units")
  (au:print-audiounits "aumu")		; calls au:get-node-strings
  (print "FX Units")
  (au:print-audiounits "aufx")
)

(au:print-graph)

(io:midi-print-sources)
(io:midi-print-destinations)

(define (split-into-lines str)
  nil)

(define (parse-params node-str)
  nil)

(define (replace-node-name node-list)
  nil)

(define (node-list node-type)
  ; create list of node lists
  (let ((str (au:get-node-strings node-type)))
    (map (lambda (node-str) (replace-node-name (parse-params node-str)))
	 (split-into-lines str))))
