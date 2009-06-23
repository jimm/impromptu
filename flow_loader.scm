;;; Loads flow files in the correct order.

;; For some reason, using load inside map/for-each does not work. Only
;; the first file gets loaded. I don't know why. Sigh.
; (for-each
;   (lambda (f) (print f)  (load-my-file (string-append f ".scm")))
;   (list "flow_data" "flow_list_gui" "flow"))

(load-my-file "flow_data.scm")
(load-my-file "flow_list_gui.scm")
(load-my-file "flow.scm")
