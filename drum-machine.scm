; ensure the audio graph is empty
(au:clear-graph)

; setup gms instrument
(define gms (au:make-node "aumu" "dls " "appl"))
(au:connect-node gms 0 *au:output-node* 0)
(au:update-graph)

; 2 * pi = one complete wave phase
(define 2-pi (* 2.0 3.1415927))

; define gm drums 
(load-my-file "midi-consts")

; step-seq is a list of 16 lists where each list is played on consecutive 16th-notes
; every pitch in a list is played simultaneously
;
; try changing the contents of the lists, and re-evaluating step-seq
(define step-seq '((*gm-closed-hi-hat* *gm-maracas* *gm-kick*)
                   (*gm-closed-hi-hat* *gm-maracas*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-snare-2*)
                   (*gm-closed-hi-hat* *gm-maracas*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-kick*)
                   (*gm-closed-hi-hat* *gm-maracas*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-hi-conga*)
                   (*gm-open-hi-hat* *gm-long-guiro* *gm-kick-2*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-kick*)
                   (*gm-closed-hi-hat* *gm-maracas*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-cabasa*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-cowbell*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-kick*)
                   (*gm-closed-hi-hat* *gm-maracas*)
                   (*gm-closed-hi-hat* *gm-maracas* *gm-pedal-hi-hat*)
                   (*gm-open-hi-hat*)))

(define play-drums
   (lambda (time step get-volume)
      (map (lambda (p)
              (play-note time gms (eval p) (get-volume step) 10000 *gm-drum-channel*))
           (list-ref step-seq (modulo step 16)))
      (callback (+ time 10000) play-drums (+ time 11025) (+ step 1) get-volume)))
 
; modulates the volume via a cosine oscillator
(play-drums (now) 0 (lambda (step) (+ 60 (* 40 (cos (* 2-pi (/ step 4)))))))