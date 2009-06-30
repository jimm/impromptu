;;; Loads flow files in the correct order.

;; Using load inside map/for-each does not work. That is because Impromptu's
;; load returns to the top level so the stack gets reset. So this code
;; won't work as expected; only the first file gets loaded. Note that this
;; limitation will be removed in the next release of Impromptu.
;;
;; (for-each
;;   (lambda (f) (load-my-file f))
;;   (list "flow-data" "flow-list-gui" "flow"))

(load-my-file "utils")
(load-my-file "flow-data")
(load-my-file "flow-list-gui")
(load-my-file "flow")
