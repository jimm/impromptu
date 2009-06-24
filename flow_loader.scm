;;; Loads flow files in the correct order.

;; Using load inside map/for-each does not work. That is because Impromptu's
;; load returns to the top level so the stack gets reset. So this code
;; won't work as expected; only the first file gets loaded.
;;
;; (for-each
;;   (lambda (f) (load-my-file (string-append f ".scm")))
;;   (list "flow_data" "flow_list_gui" "flow"))

(load-my-file "flow_data.scm")
(load-my-file "flow_list_gui.scm")
(load-my-file "flow.scm")
