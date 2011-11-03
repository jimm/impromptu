;;; ================================================================
;;; Filter functions
;;;
;;; A filter is a function that accepts its own args plus the standard
;;; MIDI event args. The filter must return either a list of (possibly
;;; modified) MIDI event args or the empty list.
;;; ================================================================

;; This is the curry function. Use it to create a filter. For example,
;; to create a filter using the xpose function (defined in flow.scm):
;;
;;   (mk-f xpose 12)
(define (mk-f fun . args)
   (lambda x
      (apply fun (append args x))))

;;; ================================================================
;;; Flow functions
;;;
;;; A flow is a list whose optional first element is a string name, next
;;; element (possibly the first, if there is no string name) is either null
;;; (the empty list) or a proc to call, and whose remaining elements are
;;; filters.
;;;
;;; Each filter must return either a list of the form '(device type chan
;;; byte-a byte-b) or the empty list. If a filter returns the empty list
;;; then the remaining filters in the flow are ignored.
;;;
;;; Example flow:
;;; '(
;;;   "Name of flow"    ; optional
;;;   (lambda ()        ; pre function list
;;;     (pre1 "arg")
;;;     (pre2 "arg"))
;;;   ; start of filter list
;;;   (mk-f filter1 "arg")
;;;   (mk-f filter2 "arg"))
;;; ================================================================

;; Returns #t if the flow has a name---that is, if the car is a string.
(define flow-has-name? (lambda (flow) (string? (car flow))))

;; Returns the empty string if the flow does not have a name.
(define flow-name (lambda (flow) (if (flow-has-name? flow) (car flow) "")))

;; Return a flow's list of pre-filter functions.
(define flow-pre-proc (lambda (flow) (if (flow-has-name? flow) (cadr flow) (car flow))))

;; Return a flow's list of filters.
(define flow-filter-list (lambda (flow) (if (flow-has-name? flow) (cddr flow) (cdr flow))))
