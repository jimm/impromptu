;;; This is the beginning of a simple sequencer. Maybe.
;;;
;;; Requires metronome.scm, which in turn requires midi-setup.scm and
;;; midi-consts.scm for the {start,stop}-clicks functions..

;; TODO
;;
;; - remember beginning offset when recording a track; apply it to first
;;   element.
;;
;; - is-recording flag so stop-recording will do nothing (not screw up
;;   *recording* if not recording

;; Stores input and output information during recording. A list of the form
;; (from-device to-device to-channel). Merged with *recording* data by
;; midi-stop-recording.
(define *recording-info* ())

;; Accessor methods
(define recording-src (lambda () (car *recording-info*)))
(define recording-dest (lambda () (cadr *recording-info*)))
(define recording-chan (lambda () (caddr *recording-info*)))

;; Stores MIDI data during recording. Each element contains (delta-time type a
;; b). During recording, times are absolute and the order of the events is
;; backwards (most recent event is first). After recording, they are changed
;; to delta-times and merged with *recording-info* to become a track (see
;; midi-stop-recording). *recording* is not changed by that operation.
(define *recording* ())

; Stores old io:midi-in value during recording. This value is restored when
; recording stops.
(define *old-io-midi-in* ())

; metronome tempo, bpm
(define *tempo* 120)

; device, channel, note
(define *metronome* (list 0 *gm-drum-channel* *gm-closed-hi-hat*))

; Adds (absolute-time type a b) to the front of *recording*.
(define add-to-recording
  (lambda (t dev type to-chan a b)
    (set! *recording* (cons (list t type a b) *recording*))))

; helper to do-calc-delta-times
(define event-with-delta-time
  (lambda (event prev-time)
    (cons (if (zero? prev-time) 0 (- (car event) prev-time))
          (cdr event))))

; helper to calc-delta-times
(define do-calc-delta-times
  (lambda (list prev-time event rest)
    (let ((new-list (cons (event-with-delta-time event prev-time) list)))
      (if (null? rest) (reverse new-list)
          (do-calc-delta-times new-list (car event) (car rest) (cdr rest))))))

; Given a list like *recording* return a list with absolute times converted to
; delta times.
; FIXME
(define calc-delta-times
  (lambda (list)
    (if (null? list) ()
        (do-calc-delta-times () 0 (car list) (cdr list)))))

;; ================ playing ================

;; Play a track using the device and channel built in to the track.
(define play-track
  (lambda (track)
    (let ((dev (car track))
          (chan (cadr track))
          (events (caddr track)))
      (play-track-on track dev chan))))

;; Play a track ignoring the device and channel built-in, instead using the
;; device and channel given.
(define play-track-on
  (lambda (track dev chan)
    (play-track-event-list dev chan (caddr track))))

;; Helper for play-track-event-list
(define do-play-track-event-list
  (lambda (dev chan event rest)
    (let ((delta-time (car event))
          (type (cadr event))
          (a (caddr event))
          (b (cadddr event)))
      (io:midi-out (now) dev type chan a b)
      (when (not (null? rest))
        (callback (+ (now) (caar rest))
                  do-play-track-event-list dev chan (car rest) (cdr rest))))))

;; Play a track event list ((delta type a b) (delta type a b)...) using the
;; specifeed device and channel.
(define play-track-event-list
  (lambda (dev chan events)
    (do-play-track-event-list dev chan (car events) (cdr events))))

;; ================ recording ================

;; Method used for MIDI input. Echoes input and calls add-to-recording.
(define midi-record
  (lambda (dev type chan a b)
    (let ((from (recording-src))
          (to (recording-dest))
          (to-chan (recording-chan)))
      (when (or (null? from) (equal? dev from))
        (io:midi-out (now) to type to-chan a b)
        (add-to-recording (now) to type to-chan a b)))))

;; Called by midi-start-recording to make sure that the *metronome* output
;; device value is defined.
(define ensure-metronome-defined
  (lambda ()
    (when (or (null? *metronome*) (zero? (cadr *metronome*)))
      (print "Must define *metronome*. For example,")
      (print "  (define *metronome* (list device *gm-drum-channel* *gm-closed-hi-hat*))")
      (error "undefined *metronome* device"))))

;; Start recording into *recording*.
(define midi-start-recording
  (lambda (from to to-chan . tracks-to-play)
    (ensure-metronome-defined)
    (set! *recording-info* (list from to to-chan))
    (set! *recording* ())
    (set! *old-io-midi-in* io:midi-in)
    (set! io:midi-in
          (lambda (dev type chan a b)
            (midi-record dev type chan a b)))
    (start-metronome
     (car *metronome*) (cadr *metronome*) (caddr *metronome*) *tempo*
     (lambda (dev chan note)
       (io:midi-out (now) dev *io:midi-on* chan note 127)))
    (when (not (null? tracks-to-play))
      (map play-track (car tracks-to-play)))))

;; Clean up *recording* (reverse it and turn absolute times into delta times)
;; and merge with *recording-info* to return a track of the form (out-device
;; channel (events...)).
(define make-track-from-recording
  (lambda ()
    (list (recording-dest) (recording-chan)
          (calc-delta-times (reverse *recording*)))))

;; Stop recording, cleans up *recording*, and returns a track which is a list
;; of the form (out-device channel (events...)). Each event is a list of the
;; form (delta-time type a b).
;;
;; Note that the caller must add device and output channel to the track info
;; on playback.
(define midi-stop-recording
  (lambda ()
    (set! io:midi-in *old-io-midi-in*)
    (stop-metronome)
    (make-track-from-recording)))
