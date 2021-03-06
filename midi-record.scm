;;; This is the beginning of a simple sequencer. Maybe.
;;;
;;; Requires utils.scm and metronome.scm, which in turn requires
;;; midi-setup.scm and midi-consts.scm for the {start,stop}-clicks
;;; functions.

(require "utils" "metronome")

;; TODO
;;
;; - use seq tempo to play tracks, which means changing delta times from
;;   milliseconds to fractions of a beat (960 ticks per beat)
;;
;; - quantize
;;
;; - names?
;;
;; - track mute and track solo

;; Stores input and output information during recording. A list of the form
;; (from-device to-device to-channel start-time). Merged with *recording* data
;; by midi-stop-recording.
(define *recording-info* ())

;; Accessor methods
(define recording-src (lambda () (car *recording-info*)))
(define recording-dest (lambda () (cadr *recording-info*)))
(define recording-chan (lambda () (caddr *recording-info*)))
(define recording-start-time (lambda () (cadddr *recording-info*)))

;; Stores MIDI data during recording. Each element contains (delta-time type a
;; b). During recording, times are absolute and the order of the events is
;; backwards (most recent event is first). After recording, they are changed
;; to delta-times and merged with *recording-info* to become a track (see
;; midi-stop-recording). *recording* is not changed by that operation.
(define *recording* ())

;; True when playing. Set to #f to stop playing.
(define *playing* #f)

; Stores old io:midi-in value during recording. This value is restored when
; recording stops.
(define *old-io-midi-in* ())

; device, channel, note. Tempo is set using
; (*metro* 'set-tempo 120)
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
(define calc-delta-times
  (lambda (abs-start-time list)
    (if (null? list) ()
        (do-calc-delta-times () abs-start-time (car list) (cdr list)))))

;; ================ tracks ================

;; A track is an alist with name, dest, chan, and events.

(define make-track
  (lambda (name dest chan)
    (list
     (cons "name" name)
     (cons "dest" dest)
     (cons "chan" chan)
     (cons "events" ()))))

;; Clean up *recording* (reverse it and turn absolute times into delta times)
;; and merge with *recording-info* to return a track of the form (out-device
;; channel (events...)).
(define make-track-from-recording
  (lambda (name)
    (list
     (cons "name" name)
     (cons "dest" (recording-dest))
     (cons "chan" (recording-chan))
     (cons "events" (calc-delta-times (recording-start-time) (reverse *recording*))))))

(attr-accessor track name)
(attr-accessor track dest)
(attr-accessor track chan)
(attr-accessor track events)

;; ================ sequences ================

;; A sequence is an alist with tempo and tracks. We return a sequence
;; with the current *tempo* value and an empty track list.
(define make-seq
  (lambda ()
    (list
     (cons "tempo" *tempo*)
     (cons "tracks" ()))))

(attr-accessor seq tempo)
(attr-accessor seq tracks)

(define seq-add-track!
  (lambda (seq track)
    (seq-set-tracks! (cons track (seq-tracks seq)))))

(define seq-play (lambda (seq) (play-tracks (seq-tracks seq))))

;; ================ playing tracks ================

;; Play a track using the device and channel built in to the track. See also
;; play-track-on.
(define play-track
  (lambda (beat track)
    (let ((dev (track-dest track))
          (chan (track-chan track)))
      (play-track-on track beat dev chan))))

;; Play all tracks in track-list, using the device and channel built in to
;; each track.
(define play-tracks
  (lambda (track-list)
    (let ((start-beat (*metro* 'get-beat 1)))
      (map (lambda (t) (play-track start-beat t)) track-list))))

(define stop-playing (lambda () (set! *playing* #f)))

;; Play a track ignoring the device and channel built-in, instead using the
;; device and channel given.
(define play-track-on
  (lambda (track beat dev chan)
    (set! *playing* #t)
    (play-track-event-list beat dev chan (track-events track))))

;; Helper for play-track-event-list
(define do-play-track-event-list
  (lambda (dev chan event rest)
    (if *playing*
      (let ((delta-time (car event))
            (type (cadr event))
            (a (caddr event))
            (b (cadddr event)))
        (io:midi-out (now) dev type chan a b)
        (if (not (null? rest))
            (callback (+ (now) (caar rest))
                      do-play-track-event-list dev chan (car rest) (cdr rest))
            (io:midi-out (now) dev *io:midi-cc* chan *cm-all-notes-off* 0)))
      (io:midi-out (now) dev *io:midi-cc* chan *cm-all-notes-off* 0))))


;; Play a track event list ((delta type a b) (delta type a b)...) using the
;; specifeed device and channel.
(define play-track-event-list
  (lambda (beat dev chan events)
    (callback (+ (*metro* 'get-time beat) (caar events))   ; wait for first event start
      do-play-track-event-list dev chan (car events) (cdr events))))

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
    (if (null? *recording-info*)
        (begin
          (ensure-metronome-defined)
          (set! *old-io-midi-in* io:midi-in)
          (set! io:midi-in
                (lambda (dev type chan a b)
                  (midi-record dev type chan a b)))
          (set! *recording* ())
          (set! *recording-info* (list from to to-chan (now)))
          (start-metronome
           (car *metronome*) (cadr *metronome*) (caddr *metronome*)
           (lambda (beat dev chan note)
             (io:midi-out (*metro* 'get-time beat) dev *io:midi-on* chan note 127)))
          (when (not (null? tracks-to-play))
                (map play-track (car tracks-to-play))))
        (print "midi-start-recording: already recording; ignoring request"))))

;; Stop recording, cleans up *recording*, and returns a track which is a list
;; of the form (out-device channel (events...)). Each event is a list of the
;; form (delta-time type a b).
;;
;; Note that the caller must add device and output channel to the track info
;; on playback.
(define midi-stop-recording
  (lambda ()
    (set! io:midi-in *old-io-midi-in*)
    (stop-playing)
    (stop-metronome)
    (let ((track (make-track-from-recording "New Track")))
      (set! *recording-info* ())
      track)))
