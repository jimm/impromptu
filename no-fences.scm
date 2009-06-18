;; Ensure the audio graph is empty
(au:clear-graph)

;; Setup gms instrument
(define gms (au:create-node "aumu" "dls " "appl"))
(au:connect-node gms 0 *au:output-node* 0)
(au:update-graph)

;; A chunk is a list of the form 

whose car is "chunk" and whose cdr is a list of
;; chunks

(define *chan-prog-alist*
  '((0 . *gm-pc-acoustic-grand-piano*)	; piano chords
    (1 . *gm-pc-electric-bass-finger*)  ; bass 1
    (2 . *gm-pc-synth-bass-1*)		; bass 2
    (3 . *gm-pc-brass-section*)		; brass
    (4 . *gm-pc-bright-acoustic-piano*) ; piano solo
    (5 . *gm-pc-percussive-organ*)	; organ melody
    (6 . *gm-pc-electric-guitar-muted*) ; picky guitar
    (7 . *gm-pc-pad-2-warm*)		; wash
    (8 . *gm-pc-tenor-sax*)		; saxes
))					; chan 9 is drums

;; send program changes
(define (setup t)
  (map (lambda (chan-prog-alist)
	 (au:midi-out t gms *io:midi-pc*
		      (car chan-prog-alist) (cdr chan-prog-alist)))
       *chan-prog-alist*))

(define-macro (drums t p v)
  `(play-note t gms p v 1000 9))

(define-macro (bass t p v d)
  `(play-note t gms p v d 1))

(define-macro (piano t p v d)
  `(play-note t gms p v d 0))

; (define 

(define (play-song chunk-list)
  (map (lambda (chunk) (play-chunk chunk)) chunk-list))

; (define (play-chunk chunk)

(play-song '(intro verse verse chorus verse chorus solo ending))
