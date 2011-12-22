;;; ================================================================
;;; Simple pass-throughs
;;; ================================================================

;; A function that returns a pass-through function that can
;; be used as a value for io:midi-in.
;;
;; Example:
;;   (define io:midi-in (pass-through *mb* *kz* 0))
(define pass-through
   (lambda (from to to-chan)
      (lambda (dev type chan a b)
         (when (= dev from)
               (io:midi-out (now) to type to-chan a b)))))

;;; ================================================================
;;; Playing a flow
;;; ================================================================

;; Play a list of fiters. Stop processing as soon as a filter returns
;; the empty list.
(define play-filters
   (lambda (list-of-filters midi-arg-list)
      (when (not (null? list-of-filters))
            (let* ((f (car list-of-filters))
                   (new-midi-arg-list (apply f midi-arg-list)))
               (when (not (null? new-midi-arg-list))
                     (play-filters (cdr list-of-filters) new-midi-arg-list))))))

;; Play a flow by running each of the pre-filter functions then calling
;; play-filters with the list of filters. First arg is a flow. Optional
;; remaining args are a function and args to call on every MIDI event.
;; (This function is used to start playing another flow, redefining
;; io:midi-in and taking over the flow-playing process.)
(define play-flow
   (lambda args
      (let* ((flow (car args))
             (pre-func (flow-setup-funcs flow))
             (interrupt (cdr args)))
        (when (not (null? pre-func)) (apply pre-func)) ; Run pre-filter proc
        (set! io:midi-in
              (lambda (dev type chan a b)
                 (when (not (null? interrupt))
                       (apply (eval (car interrupt)) dev type chan a b (cdr interrupt)))
                 (play-filters (flow-filter-list flow) (list dev type chan a b)))))))

;;; ----------------------------------------------------------------
;;; Playing a list of flows, moving between them in response to
;;; particular MIDI events.
;;; ----------------------------------------------------------------

;; Returns #t if the args define a MIDI event that we recognize for moving to
;; the previous flow.
(define prev-flow-event-p
   (lambda (dev type chan a b)
      (and (= type *io:midi-cc*)
           (= chan 0)
           (= a 110)
           (= b 127))))

;; Returns #t if the args define a MIDI event that we recognize for moving to
;; the next flow.
(define next-flow-event-p
   (lambda (dev type chan a b)
      (and (= type *io:midi-cc*)
           (= chan 0)
           (= a 111)
           (= b 127))))

;; Look for prev-flow-event-p or next-flow-event-p and play prev/next flow
;; if appropriate.
(define play-flow-list-interrupt-func
   (lambda (dev type chan a b canvas n flow-list)
      (when (and (prev-flow-event-p dev type chan a b)
                 (> n 0))
            (play-nth-flow-list canvas (- n 1) flow-list))
      (when (and (next-flow-event-p dev type chan a b)
                 (< (+ n 1) (length flow-list)))
            (play-nth-flow-list canvas (+ n 1) flow-list))))

;; Given an index n and a list of flows, play the nth flow.
(define play-nth-flow-list
   (lambda (canvas n flow-list)
      (draw-flow-list canvas n flow-list)
      (play-flow (list-ref flow-list n) play-flow-list-interrupt-func canvas n flow-list)))

;; Hold on to a list of flows. Play the first one and listen for MIDI
;; events that tell us to move to the next/prev flow in the list.
;; Calls methods that create and update *flow-list-canvas*. It's OK
;; if *flow-list-canvas* is null.
;;
;; See prev-flow-event-p and next-flow-event-p. You can redefine them.
(define play-flow-list
   (lambda flow-list
      (play-nth-flow-list *flow-list-canvas* 0 flow-list)))

;;; ================================================================
;;; Pre-defined pre-filter functions
;;; ================================================================

;; (pc dest chan val)
;; Program change.
(define pc
   (lambda (dest chan val)
      (io:midi-out (now) dest *io:midi-pc* chan val)))

;; (vol dest chan val)
;; Output a volume CC command to destination on channel.
(define vol
   (lambda (dest chan val)
      (io:midi-out (now) dest *io:midi-cc* chan 7 val)))

;; (full-vol dest chan)
;; Output a volume CC command to destination on channel with val 127.
(define full-vol (lambda (dest chan) (vol dest chan 127)))

;; (kz-pc chan program)
;; Kurzweil program change. Sends bank (100's value) then program (0-99).
;;
;; Example:
;;   (kz-pc 0 93)
(define kz-pc
   (lambda (chan program)
      (let* ((bank (+ 100 (real->integer (rational->real (/ program 100)))))
             (prog (modulo program 100)))
         (pc *kz* chan bank)
         (pc *kz* chan prog))))

;; (ws-bank chan bank)
;; Wavestation bank change. Only two bank values make sense:
;;   0 = RAM1/RAM2
;;   1 = ROM/CARD
;;
;; Example:
;;   (ws-bank 5 1)
(define ws-bank
   (lambda (chan bank)
      (io:midi-out (now) *ws* *io:midi-cc* chan 0 0)
      (io:midi-out (now) *ws* *io:midi-cc* chan #x20 bank)))

;; (ws-pc chan bank pc)
;; Wavestation bank and program change. Only two bank values make sense:
;;   0 = RAM1/RAM2
;;   1 = ROM/CARD
;;
;; Example:
;;   (ws-bank-pc 5 1)
(define ws-pc
   (lambda (chan bank program)
      (ws-bank chan bank)
      (pc *ws* chan program)))

;;; ================================================================
;;; Pre-defined filters
;;;
;;; Filters return either (dev type chan a b) or the empty list.
;;; If a filter returns the empty list, then no filters after that
;;; will run for that MIDI event.
;;; ================================================================

;; (only-from input-device)
;; Only let through messages from input-device.
(define only-from
   (lambda (in-dev dev type chan a b)
      (if (equal? in-dev dev)
          (list dev type chan a b)
          ())))

;; (xpose amount)
;; Transpose note events by amount.
(define xpose
   (lambda (amt dev type chan a b)
      (list dev type chan (if (note-type? type) (+ a amt) a) b)))

;; (range low high)
;; Only let through note events between low and high inclusive.
;; All other events are let through.
(define range
   (lambda (low high dev type chan a b)
      (let ((midi-args (list dev type chan a b))
            (low-val (if (string? low) (string->note low) low))
            (high-val (if (string? high) (string->note high) high)))
         (if (note-type? type)
             (if (and (>= a low-val) (<= a high-val))
                 midi-args
                 ())
             midi-args))))

;; (out destination channel)
;; Output all events to destination on channel.
(define out
   (lambda (out-dest out-chan in-dev type in-chan a b)
      (io:midi-out (now) out-dest type out-chan a b)
      (list in-dev type in-chan a b)))

;; (multi (list (list filters...)
;;              (list filters...)))
;; Run all filter lists in the list.
(define multi
    (lambda (filter-list-list dev type chan a b)
       (let ((midi-args (list dev type chan a b)))
          (for-each (lambda (filter-list) (play-filters filter-list midi-args))
                    filter-list-list))))

;; (block match-assoc)
;; Blocks further processing of MIDI events that match the values in assoc.
;; The keys that are used are 'type, 'chan, 'a, and 'b.
;;
;; Example:
;;   (block '((type . *io:midi-cc*) (a . 7)))
;; blocks volume.

; Return non-#f if value in match-data for key is equal to val. It's OK if
; there is no entry for key.
(define block-match
   (lambda (key match-data-assoc val)
      (let ((a (assoc key match-data-assoc)))
         (if (not a)
             #t
             (equal? (cdr a) val)))))

(define block
   (lambda (match-data-assoc dev type chan a b)
      (if (and (block-match 'type match-data-assoc type)
               (block-match 'chan match-data-assoc chan)
               (block-match 'a match-data-assoc a)
               (block-match 'b match-data-assoc b))
          ()
          (list dev type chan a b))))
