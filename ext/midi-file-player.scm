;;; This is from an email by Andrew Sorensen to the Impromptu mailing list on
;;; 2009-10-25. It is example code, not a ready-to-use library.
;;;
;;; Requires Impromptu 2.0.


;; dls to play midi with
(define dls (au:make-node "aumu" "dls " "appl"))
(au:connect-node dls 0 *au:output-node* 0)
(au:update-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first define some helper functions
;; to make life easier

(define objc:object-for-key
   (lambda (dictionary key)
      (objc:call dictionary "objectForKey:" key)))

(define objc:object-at-index
   (lambda (array index)
      (objc:call array "objectAtIndex:" index)))

(define objc:number-at-index
   (lambda (array index)
      (objc:nsnumber->number (objc:call array "objectAtIndex:" index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; now for the real work
;;

;; MIDI File data is read in as an NSDictionary
;; at this stage the dictionary only contains to
;; key value pairs:
;; "pchangedata": a key that returns an NSArray
;; of program changes (one per track)
;;
;; "trackdata": a key that returns an NSArray
;; of tracks.  Each track then contains an
;; NSArray of notes.  Each note is an nsarray
;; of four numbers [start-beat,pitch,volume,duration]


;; read in midi files as NSDictionary
(define midi-data (io:read-midi-file-data "/tmp/spring.mid"))
;; get program change data
(define pchanges (objc:object-for-key midi-data "pchangedata"))
;; get midi tracks from dictionary as NSArray of NSArray's
(define tracks (objc:object-for-key midi-data "trackdata"))

;; play back track WITHOUT scheduling into the future
;; (so you can change tempo on the fly etc.)
(define play-track
   (lambda (beat track note index channel)
      (play dls
            (objc:number-at-index note 1)
            (objc:number-at-index note 2)
            (objc:number-at-index note 3)
            channel)
      (if (< index (objc:call track "count"))
          (let* ((nextnote (objc:object-at-index track index))
                 (dur (- (objc:number-at-index nextnote 0)
                         (objc:number-at-index note 0))))
             (callback (*metro* (+ beat (* .5 dur))) 'play-track (+ beat dur)
                       track
                       nextnote
                       (+ index 1)
                       channel)))))

;; set a tempo to start at
;; try changing this halfway through playback
(*metro* 'set-tempo 110)

;; play all tracks first setting each tracks program change (if available)
(let ((shared-start-beat (*metro* 'get-beat 1)))
   (dotimes (i (objc:call tracks "count"))
      ;; set program change
      (au:midi-out (now) dls *io:midi-pc* 0 (objc:number-at-index pchanges i) 0)
      (let* ((track (objc:object-at-index tracks i))
             (first-note (objc:object-at-index track 0)))
         (play-track (+ shared-start-beat
                        (objc:number-at-index first-note 0))
                     track first-note 1 i))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; little example writing out midi data
(define midi-data-out (objc:call "NSMutableDictionary" "dictionary"))
(define tracks-out (objc:call "NSMutableArray" "array"))
(define track1 (objc:call "NSMutableArray" "array"))
(define track2 (objc:call "NSMutableArray" "array"))
(define track3 (objc:call "NSMutableArray" "array"))
(define track4 (objc:call "NSMutableArray" "array"))

;; define helper function
(define add-note
   (lambda (track beat pitch volume duration)
      (objc:call track "addObject:"
                 (objc:list->nsarray (list beat pitch volume duration)))))


;; an amazingly uninteresting midi file generator
(define generator
   (lambda (beat)
      (let ((chord (pc:make-chord 40 70 3 (random '((0 3 7)
                                                    (8 0 10)
                                                    (7 11 2)
                                                    (5 8 0))))))-
         (add-note track2 beat (- (car chord) 12) 50 1.8)
         (add-note track2 (+ beat 1/2) (car chord) 50 1.4)
         (add-note track3 (+ beat 1) (cadr chord) 50 0.8)
         (add-note track4 (+ beat 3/2) (caddr chord) 50 0.4)
         (let* ((dur1 (* 2 (random '(1/2 2/3 3/4 1/4 1/3))))
                (dur2 (- 2 dur1))
                (pitch (pc:random 60 80 '(0 2 3 7 8))))
            (add-note track1 beat pitch 60 (* .9 dur1))
            (if (> dur2 0)
                (add-note track1 (+ beat dur1)
                          (pc:relative pitch (random '(-1 1)) '(0 2 3 5 7 8 10))
                          80 (* .9 dur2)))))
      (if (< beat 50) (generator (+ beat 2)))))

;; populate track data
(generator 0)

;; add track data to tracks-out
(objc:call tracks-out "addObject:" track1)
(objc:call tracks-out "addObject:" track2)
(objc:call tracks-out "addObject:" track3)
(objc:call tracks-out "addObject:" track4)

;; add tracks out to midi-data-out
(objc:set-value-for-key midi-data-out "trackdata" tracks-out)

;; write data out to midi file
(io:write-midi-file-data midi-data-out "/tmp/amazing.mid")
