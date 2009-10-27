;; Examples, from simplest (most default values) to most complex (no default
;; values except for note velocity):
;;
;;   (start-clicks 120)
;;
;;   (start-midi-metronome-std *dv* 120)
;;
;;   (start-midi-metronome *dv* *gm-drum-channel* *gm-closed-hi-hat* 120)
;;
;;   (start-metronome *dv* *gm-drum-channel* *gm-closed-hi-hat* 120
;;     (lambda (time dev chan note)
;;       (io:midi-out time dev *io:midi-on* chan note 127)))))

;; TODO
;;
;; - Use make-metro
;;
;; - add num beats per measure, make metronome louder at beginning of measures

(define metronome-on #f)

;; Calls a function regularly, passing in dev, chan, note.
(define metronome
  (lambda (time dev chan note tempo func)
    (when metronome-on
      (func time dev chan note)         ; do something at time
      (let ((t (+ time (/ (* 60 *second*) tempo))))
        (callback (- t 500) metronome t dev chan note tempo func)))))

;; Arguments: MIDI device, channel, note, tempo, and callback function.
(define start-metronome
  (lambda (dev chan note tempo func)
    (set! metronome-on #t)
    (metronome (now) dev chan note tempo func)))

(define stop-metronome
  (lambda ()
    (set! metronome-on #f)))

;; Arguments: MIDI device, channel, note, and tempo.
(define start-midi-metronome
  (lambda (dev chan note tempo)
    (start-metronome
     dev chan note tempo 
     (lambda (time dev chan note) (io:midi-out time dev *io:midi-on* chan note 127)))))

(define stop-midi-metronome stop-metronome)

;; Two argument: MIDI device and tempo. Assumes both midi-setup.scm and
;; midi-consts.scm have been loaded.
(define start-midi-metronome-std
  (lambda (dev tempo)
    (start-midi-metronome dev *gm-drum-channel* *gm-closed-hi-hat* tempo)))

(define stop-midi-metronome-std stop-metronome)

;; One argument; tempo. Uses the metronome to play clicks using my D4. Assumes
;; both midi-setup.scm and midi-consts.scm have been loaded.
(define start-clicks (lambda (tempo) (start-midi-metronome-std *d4* tempo)))

(define stop-clicks stop-metronome)
