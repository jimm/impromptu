;; Uses *metro*. To set tempo, (*metro* 'set-tempo 120).
;;
;; Examples, from simplest (most default values) to most complex (no default
;; values except for note velocity):
;;
;;   (start-clicks)
;;
;;   (start-midi-metronome-std *dv*)
;;
;;   (start-midi-metronome *dv* *gm-drum-channel* *gm-closed-hi-hat*)
;;
;;   ; note that beat is passed to func, not time
;;   (start-metronome *dv* *gm-drum-channel* *gm-closed-hi-hat*
;;     (lambda (beat dev chan note)
;;       (io:midi-out (*metro* 'get-time beat) dev *io:midi-on* chan note 127)))))

;; TODO
;;
;; - add num beats per measure, make metronome louder at beginning of measures

(define metronome-on #f)

;; Calls a function regularly, passing in dev, chan, note.
(define metronome
  (lambda (beat dev chan note func)
    (when metronome-on
      (let ((time (*metro* 'get-time beat)))
        (func beat dev chan note)           ; do something at time
        (let ((dur (*metro* 'dur 1)))
          (callback (+ time (* dur 0.5)) metronome (+ 1 beat) dev chan note func))))))

;; Arguments: MIDI device, channel, note, and callback function.
(define start-metronome
  (lambda (dev chan note func)
    (set! metronome-on #t)
    (metronome (*metro* 'get-beat 1) dev chan note func)))

(define stop-metronome
  (lambda ()
    (set! metronome-on #f)))

;; Arguments: MIDI device, channel, and note.
(define start-midi-metronome
  (lambda (dev chan note)
    (start-metronome
     dev chan note
     (lambda (beat dev chan note) (io:midi-out (*metro* 'get-time beat) dev *io:midi-on* chan note 127)))))

(define stop-midi-metronome stop-metronome)

;; Two argument: MIDI device. Assumes midi-consts.scm has been loaded.
(define start-midi-metronome-std
  (lambda (dev)
    (start-midi-metronome dev *gm-drum-channel* *gm-closed-hi-hat*)))

(define stop-midi-metronome-std stop-metronome)

;; Uses the metronome to play clicks using my D4. Assumes both midi-setup.scm
;; and midi-consts.scm have been loaded.
(define start-clicks (lambda () (start-midi-metronome-std *d4*)))

(define stop-clicks stop-metronome)
