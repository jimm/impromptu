;; only need to load midi-consts if it isn't in ~/Library/Application Support/Impromptu
;; (load "midi-consts.scm")

;; Returns list of lists. Each outer list is a track. A track consists of
;; start-times, pitches, volumes, and durations (in beats).
;; Tracks are returned in reverse order from MIDI file.
;; (define midi-data
;;     (io:read-midi-file "/Users/jimm/src/jimm/midilib/examples/NoFences.mid"))

;; List of track metadata. (name midi-channel gm-program-change).
;; (define track-metadata
;;    '(("saxes" 8 *gm-pc-tenor-sax*)
;;      ("wash" 7 *gm-pc-pad-2-warm*)
;;      ("picky guitar" 6 *gm-pc-electric-guitar-muted*)
;;      ("piano solo" 4 *gm-pc-bright-acoustic-piano*)
;;      ("organ melody" 5 *gm-pc-percussive-organ*)
;;      ("brass" 3 *gm-pc-brass-section*)
;;      ("piano chords" 0 *gm-pc-acoustic-grand-piano*)
;;      ("bass 2" 2 *gm-pc-synth-bass-1*)
;;      ("bass 1" 1 *gm-pc-electric-bass-finger*)
;;      ("drums" 9 nil)
;;      ("conductor" nil nil))

(define (metadata-name metadata) (car metadata))
(define (metadata-channel metadata) (cadr metadata))
(define (metadata-pc metadata) (caddr metadata))

;; Ensure the audio graph is empty
(au:clear-graph)

;; Setup gms instrument
(define gms (au:create-node "aumu" "dls " "appl"))
(au:connect-node gms 0 *au:output-node* 0)
(au:update-graph)

;; Play the MIDI data. Each channel has a mark that is the same number.
(define (midi-player time instrument midi-data track-metadata)
   ; send program changes
   (map (lambda (metadata)
           (when (metadata-channel metadata)
                 (au:midi-out time instrument *io:midi-pc* (metadata-channel metadata) (metadata-pc metadata) 0)))
        track-metadata)
   ; play tracks
   (map (lambda (track metadata)
           ; allow 1/2 second for program changes to take effect
           (play-track (cons (+ time (* 0.5 *second*))
                             (metadata-channel metadata))
                       instrument
                       track
                       (metadata-channel metadata)))
        midi-data
        track-metadata))

;; Play a single track
(define (play-track time instrument track channel)
   (map (lambda (t p v r)
           (play-note (+ time (* t *second*)) instrument p v (* r *second*) channel))
        (cl:butnthcdr 5 (list-ref track 0)) ; DEBUG only play first 5
        (cl:butnthcdr 5 (list-ref track 1))
        (cl:butnthcdr 5 (list-ref track 2))
        (cl:butnthcdr 5 (list-ref track 3))))

;; Go, go Gadget!
(midi-player (now) gms (cl:butlast midi-data 1) track-metadata)

;; Play a single track
(midi-player (now) gms (list (list-ref midi-data 0)) (list (list-ref track-metadata 0)))

(list-ref midi-data 2)
(print (list-ref track-metadata 2))

;;
;; Printing
;;

(define (note-name p)
   (string-append
     (list-ref '("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B") (modulo p 12))
     (number->string (floor (/ p 12)))))

;; NOTES
;;
;; merge midi-data with metadata?
(list '(a b c) '(d e f))
(list-tail '(a b c d) 2)

;; DEBUG

(play-note (now) gms 60 120 *second* 0)

(cl:butnthcdr 3 '(a b c d e f g))

(define (print-track-debug-info t metadata)
   (print "track debug info" metadata)
   (dotimes (i 5) (print-across-track t i)))

(define (print-across-track t n)
      (print n ":"
          "start =" (list-ref (list-ref t 0) n)
          "pitch =" (note-name (list-ref (list-ref t 1) n))
          "vol =" (list-ref (list-ref t 2) n)
          "len =" (list-ref (list-ref t 3) n)))

(map (lambda (t m) (print-track-debug-info t m)) midi-data track-metadata)

(define pitches '(60 62 64 65 67 69 71 72))
(map (lambda (p) (print (note-name p))) pitches)

(print (now))
(print gms)
(print (list-ref midi-data 1))
(print track-metadata)
(print (list-ref (car (car midi-data))) 400)
(print (length (cl:butlast midi-data 1)) (length track-metadata))
(print (cl:butlast midi-data 1))
