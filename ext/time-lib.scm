; creates a rhythm of a given length with a
; simple gestalt grouping. Rhythmic atoms are 
; selected from the list provided in the function 
; or additionally as a single list argument to the function
;
; (define rlist (make-rhythm 1.0 (list (/ 1 3) 0.5 1.0)))
;
(define make-rhythm
   (lambda (beats . args)
      (define rlist 
         (if (list? (car args))
             (car args)
             (cl:remove 'bad
                    (map (lambda (i)
                            (if (null? args)
                                (if (<= i beats) i 'bad)                                
                                (if (or (< i (car args)) 
                                        (> i (cadr args)))
                                    'bad
                                    i)))                                        
                         (list .25 (/ 1 3) .5 (/ 2 3) .75 1.0 1.25 (/ 4 3) 1.5 1.75 2.0 2.5 3.0 3.5 4.0)))))
      (let loop ((cnt 0)
                 (rl (list (random rlist))))
         (cond ((null? rl)
                (loop 0 (list (random rlist))))
               ((> cnt 50) (loop 0 (list (random rlist))))
               ((= (apply + rl) beats)
                (reverse rl))                   
               ((> (apply + rl) beats)
                (loop (+ cnt 1) (cdr rl)))
               ((< (random) .35)
                (loop (+ cnt 1) (cons (car rl) rl)))
               (else (loop (+ cnt 1) (cons (random rlist)
                                 rl)))))))


; accepts an associative list as the timeline argument
; returns an event at a given time from the alist
(define make-timeline
   (lambda (timeline)
      (if (null? timeline)
          '()
          (lambda (time)
             (let loop ((lst (reverse timeline)))
                (cond ((null? lst) '())
                      ((>= time (caar lst))
                       (cdar lst))
                      (else (loop (cdr lst)))))))))


; creates a meter where metre is a list of numerators 
; and base is a shared denominator (relative to impromptu beats. i.e. 1 = crotchet,  0.5 = eighth etc.)
; 
; e.g.  (define *metre* (make-metre '(2 3 2) 0.5)) = 2/8 3/8 2/8 rotating cycle.
; 
; then call meter with time and beat 
; if beat matches time then #t else #f
;
; e.g. give the above define
;      (*metre* 2.5 1.0) => #t because 0.0 = 1, 0.5 = 2, 1.0 = 1, 1.5 = 2, 2.0 = 3, 2.5 = 1, 3.0 = 2 and repeat.
(define make-metre
   (lambda (metre base)
      (let ((metre-length (apply + metre)))
         (lambda (time beat)
            (if (= (let loop ((qtime (fmod (/ time base) metre-length))
                              (lst metre)
                              (valuea (car metre))
                              (valueb 0))
                      (if (< qtime valuea)
                          (+ 1.0 (- qtime valueb))
                          (loop qtime (cdr lst) (+ valuea (cadr lst)) (+ valueb (car lst)))))
                   beat)
                #t
                #f)))))

; creates a metronome object
; metro is basically a linear function that returns
; a time in absolute samples when given a time in beats.
;
; metro is instantiated with a starting tempo.
; you can call the metro with the following symbols
;
; 'get-time ; which is also the default
; 'get-beat 
; 'get-tempo
; 'set-tempo
; 'dur 
; 
(define make-metro
   (lambda (start-tempo . args)
      (let* ((offset (if (null? args) (now) (car args)))
             (mark offset)
             (total-beats 0.0)
             (g-tempo (/ 60 start-tempo))
             (beat-pos (lambda (x1 y1 x2 y2)
                          (let* ((m (if (= 0 (- x2 x1)) 0 (/ (- y2 y1) (- x2 x1))))
                                 (c (- y1 (* m x1))))
                             (lambda (time)
                                (+ (* time m) c)))))
             (samp-env (beat-pos 0 0 1.0 (* g-tempo *samplerate*))))
         (lambda (sym . args)
            (cond ((number? sym)
                   (+ (samp-env sym) offset))
                  ((equal? sym 'get-time)
                   (+ (samp-env (car args)) offset))
                  ((equal? sym 'set-tempo)
                   (let ((time (if (null? (cdr args)) (now) (cadr args)))
                         (val (* *samplerate* g-tempo 0.125)))
                      (set! time (+ time (- val (fmod (- time mark) val))))
                      (set! total-beats (+ total-beats (/ (- time mark) 
                                                          (* *samplerate* g-tempo))))  
                      (set! g-tempo (/ 60 (car args)))
                      (set! mark time)
                      (set! samp-env (beat-pos total-beats
                                               (samp-env total-beats)
                                               (+ total-beats 1.0) 
                                               (+ (samp-env total-beats) (* g-tempo *samplerate*))))
                      (car args)))
                  ((equal? sym 'get-tempo) (* (/ 1.0 g-tempo) 60))
                  ((equal? sym 'dur) (* *samplerate* g-tempo (car args)))
                  ((equal? sym 'get-beat) 
                   (let ((val (+ total-beats
                                 (/ (- (now) mark)
                                    (* *samplerate* g-tempo))))
                         (quantize (if (null? args) 1.0 (car args))))                              
                      (+ val (- quantize (fmod val quantize)))))
                  (else 'bad-method-name))))))