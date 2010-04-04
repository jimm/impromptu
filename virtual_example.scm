;;; Simple example of using *io:midi-virtual-device*. If you've got Garage
;;; Band running, this will send MIDI notes to it.
;;;
;;; Unfortunately, Garage Band won't listen to different MIDI channels for
;;; each track; just the current track is played.

(define loop
  (lambda ()
     (io:midi-out (now) *io:midi-virtual-device* *io:midi-on* 0 60 80)
     (io:midi-out (+ (now) 20000) *io:midi-virtual-device* *io:midi-on* 0 60 0)
     (callback (+ (now) 30000) 'loop)))

(define loop '())

(loop)
