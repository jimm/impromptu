;;; ================================================================
;;; Constants and helper functions
;;; ================================================================

;; Missing MIDI types (high nibble of status byte, shifted down)
(define *io:midi-pp* #xa)               ; poly pressure
(define *io:midi-cp* #xc)               ; chan pressure
(define *io:midi-pb* #xd)               ; pitch bend

;; List of Impromptu MIDI types for note events (on, off, poly press)
;; Must use (list), or else list will contain symbols
(define note-types (list *io:midi-on* *io:midi-off* *io:midi-pp*))

;; Return non-false if type is one of the note types.
(define note-type? (lambda (type) (member type note-types)))

;; Stop all MIDI routing.
(define stop-midi (lambda () (set! io:midi-in ())))

;; Return val clamped inside the range low-high inclusive.
(define clamp
   (lambda (val low high)
      (if (< val low) low
          (if (> val high) high
              val))))

;; Helper for string->note.
(define string->note:offset
   (lambda (str)
      (let* ((offset (case (char-downcase (string-ref str 0))
                           ((#\c) 0)
                           ((#\d) 2)
                           ((#\e) 4)
                           ((#\f) 5)
                           ((#\g) 7)
                           ((#\a) 9)
                           ((#\b) 11)))
             (sharp-flat (case (char-downcase (string-ref str 1))
                               ((#\#) 1)  ; #
                               ((#\b) -1) ; b
                               (else 0))))
         (+ offset sharp-flat))))

;; Given a note name return the MIDI note number.
(define string->note
   (lambda (str)
      (let* ((offset (string->note:offset str))
             (octave (case offset
                           ((1 3 6 8 10) (string->number (substring str 2)))
                           (else (string->number (substring str 1)))))
             (val (+ (* (+ 1 octave) 12) offset)))
         (clamp val 0 127))))


;;; ================================================================
;;; Alist helpers and macros for defining alist accessor functions
;;; ================================================================

;; ================ assoc list get/set! ================

(define aget (lambda (alist name) (cdr (assoc name alist))))
(define aset! (lambda (alist name value) (set-obj-for-key! name value alist)))

;; ================ accessor generator macros ================

; (attr-reader typename fieldname)
(macro (attr-reader args)
  `(define ,(string->symbol
             (string-append (symbol->string (cadr args))
                            "-" (symbol->string (caddr args))))
     (lambda (,(cadr args))
       (aget ,(cadr args) ,(symbol->string (caddr args))))))

; (attr-writer typename fieldname)
(macro (attr-writer args)
  `(define ,(string->symbol
             (string-append (symbol->string (cadr args))
                            "-set-" (symbol->string (caddr args)) "!"))
     (lambda (,(cadr args) val)
       (aset! ,(cadr args) ,(symbol->string (caddr args)) val))))

; (attr-accessor typename fieldname)
(macro (attr-accessor args)
  `(begin
     (attr-reader ,@(cdr args))
     (attr-writer ,@(cdr args))))

;;; ================================================================
;;; Debug
;;; ================================================================
(define debug ())

(define debug-on
   (lambda ()
      (set! debug (lambda args (apply print (append '("debug:") args))))))

(define debug-off
   (lambda ()
      (set! debug (lambda args ()))))
