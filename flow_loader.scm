;;; Loads flow files in the correct order.

(for-each (lambda (f) (load-my-file (string-append f ".scm")))
   '("flow_data" "flow_list_gui" "flow"))
