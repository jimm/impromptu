;;; ================================================================
;;; Flow functions
;;;
;;; A flow is a list whose optional first element is a string name,
;;; next element (possibly the first, if there is no string name)
;;; is a list of pre-filter functions to call, and whose remaining
;;; elements are filters.
;;;
;;; Example flow:
;;; '(( ; pre function list
;;;     (pre1 "arg")
;;;     (pre2 "arg") )
;;;   ; start of filter list
;;;   (filter1 "arg")
;;;   (filter2 "arg"))
;;;
;;; A filter is a function that accepts its own args plus the standard
;;; MIDI event args. The filter must return either a list of (possibly
;;; modified) MIDI event args or the empty list. If a filter returns
;;; the empty list then the remaining filters in the flow are ignored.
;;; ================================================================

(define flow-has-name? (lambda (flow) (string? (car flow))))

(define flow-name (lambda (flow) (if (flow-has-name? flow) (car flow) "")))

(define flow-pre-list (lambda (flow) (if (flow-has-name? flow) (cadr flow) (car flow))))

(define flow-filter-list (lambda (flow) (if (flow-has-name? flow) (cddr flow) (cdr flow))))

;;; ================================================================
;;; Filter functions
;;; ================================================================

;; This is the curry function. Use it to create a filter. For example,
;; to create a filter using the xpose function (defined in flow.scm):
;;
;;   (filter xpose 12)
(define (filter fun . args)
   (lambda x
      (apply fun (append args x))))
