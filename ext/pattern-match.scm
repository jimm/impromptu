; Scheme port of the pattern-matching library published
; in Peter Norvig's seminal "Paradigms of Artifical
; Intelligence Programming" (1992)
;
;
; primary api:
;
;   pm-match?         - does pattern match input?
;   pm-match          - match pattern against input in the context of bindings
;   *notes*           - list of symbols that represent the basic notes
;                       ie (c c# d d# e f f# g g# a a# b)
;   *note-predicates* - associative list of (note, note-predicate) pairs
;                       eg ((c . pm-is-c) (c# . pm-is-c#) (d . pm-is-d) ...)
;
; pattern rules:
;
;   1. patterns and the inputs they are matched against are lists of Scheme atoms
;      (ie symbols, numbers, strings)
;      example patterns: (this is a pattern that matches a single input of 12 atoms)
;                        (this pattern has symbols and 3 4 and "strings")
; 
;   2. ?<name> is a pattern variable, and matches any single atom. repeated use of
;      the same pattern variable within a pattern will only match against input
;      that has the same atom in the corresponding locations
;      example pattern variables:       ?x
;                                       ?note
;      example pattern with variables:  (one ?x three)
;      example inputs that match:       (one two three)
;                                       (one impromptu three)
;      example inputs that don't match: (one three)
;                                       (one two three four)
;
;   3. (?* ?<name>) is a pattern variable that matches zero or more atoms
;      example:                   (?* ?x)
;      example pattern:           (this pattern matches (?* ?x))
;      example inputs that match: (this pattern matches)
;                                 (this pattern matches many patterns)
;
;   4. ?<name>=<note> is a special kind of pattern variable, a note variable,
;      that matches any MIDI pitch that corresponds to <note>
;      examples:                      ?=c
;                                     ?=a#
;      1st example pattern:           (this pattern matches any ?=c)
;      1st example inputs that match: (this pattern matches 60)
;      2nd example pattern:           (this pattern matches zero or more (?* ?x=c#))
;      2nd example inputs that match: (this pattern matches zero or more)
;                                     (this pattern matches zero or more 61 73 85 1)
;
;
; usage examples:
;
;   (pm-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))
;   -> ((?y fool) (?x what he is))
;
;   (pm-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
;   -> ((?x 1 2 a b))
;
;   (pm-match '(a (?* ?x) d) '(a b c d))
;   -> ((?x b c))
;
;   (pm-match '(a (?* ?x) (?* ?y) d) '(a b c d))
;   -> ((?y b c) (?x))
;
;   (pm-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d)))
;   -> ((?y d) (?x b c))
;
;   (pm-match '(?x ?op ?y is ?z (?if (equal? (?op ?x ?y) ?z))) '(3 - 4 is -1))
;   -> ((?z . 12) (?y . 4) (?op . *) (?x . 3))
;
;
;   match a series of MIDI pitches of which the first, second and third
;    must be 60, 64 and 67, the last must be 67, and the ones in between
;    must include at least one C
;   (pm-match '(60 64 67 (?* ?x) ?y (?if (pm-some pm-is-c ?x)) (?if (equal? ?y 67)))
;             '(60 64 67 60 71 59 67))
;   -> ((?y . 67) (?x 60 71 59))
;
;
;   match against any C
;   (pm-match '(?x=c) '(60))
;   -> ((?x=c . 60))
;
;
;   match against any C# (should fail)
;   (pm-match '(?x=c#) '(60))
;   -> #f
;
;
;   match against any series of C, C#
;   note that we have two _different_ variables, the full names of
;    which are ?a=c and ?a=c# (ie not just ?a)
;   (pm-match '(?a=c ?a=c#) '(60 61))
;   -> ((?a=c# . 61) (?a=c . 60))
;
;
;   match against a series of C, another (possibly different C) and
;    then the first C again
;   first pattern should succeed, second should fail
;   (pm-match '(?a=c ?b=c ?a=c) '(60 0 60))
;   -> ((?b=c . 0) (?a=c . 60))
;
;   (pm-match '(?a=c ?b=c ?a=c) '(60 0 0))
;   -> #f
;


; does pattern match input?
(define (pm-match? pattern input)
  (not (eq? (pm-match pattern input) pm-fail)))


; match pattern against input in the context of the bindings
; note that we simulate an optional argument (bindings), whose
;  default value is pm-no-bindings
(define (pm-match pattern input . bindings)
  (imp-optional-arg bindings pm-no-bindings)
  (cond ((eq? bindings pm-fail) pm-fail)
        ((pm-note-variable? pattern) (pm-match-note-variable pattern input bindings))
        ((pm-variable? pattern) (pm-match-variable pattern input bindings))
        ((pm-segment-pattern? pattern) (pm-segment-matcher pattern input bindings))
        ((pm-single-pattern? pattern) (pm-single-matcher pattern input bindings))
        ((equal? pattern input) bindings)
        ((and (pair? pattern) (pair? input)) (pm-match (pm-rest pattern)
                                                       (pm-rest input)
                                                       (pm-match (pm-first pattern)
                                                                 (pm-first input)
                                                                 bindings)))
        (else pm-fail)))


; macro for implementation of optional arguments
; arg is the argument name
; default is the default value for arg, to be used
;  if the user doesn't provide a value
(define-macro (imp-optional-arg arg default)
  `(if (equal? ,arg '())
       (set! ,arg ,default)
       (set! ,arg (car ,arg))))


; simple implementation of CL's (for item in items collect body)
;  loop form
(define-macro (imp-collect items item . body)
  `(do ((items ,items (cdr items))
        (results '()))
       ((= (length items) 0) results)
     (let ((,item (car items)))
       (set! results
             (append results
                     (list ,@body))))))


; defines a predicate function that tests whether a given MIDI
;  pitch is that of a particular note
; eg: (define-note-predicate c 12) => a definition of a function
;     pm-is-c that returns true for all pitches that divide 12
;     exactly
(define-macro (define-note-predicate note modulus)
  (let ((fn-name (string->symbol (string-append "pm-is-" (symbol->string note)))))
    `(define (,fn-name pitch)
       (cond ((list? pitch)
              (pm-every (lambda (p) (= (modulo p 12) ,modulus))
                        pitch))
             ((number? pitch)
              (= (modulo pitch 12) ,modulus))
             (else #f)))))


; defines note predicates for a list of notes, by evaluating
;  a call to define-note-predicate for each item in notes
; returns notes (as a quoted list - we're a macro, remember)
(define-macro (define-note-predicates notes)
  (do ((modulus 0 (+ modulus 1))
       (notes-remaining notes (cdr notes-remaining)))
      ((= (length notes-remaining) 0) (list 'quote notes))
    (eval `(define-note-predicate ,(car notes-remaining) ,modulus)
          (interaction-environment))))


; creates an alist of note <-> note-predicate pairs
(define (note-predicates notes)
  (if (= (length notes) 0)
      '()
      (let* ((note    (car notes))
             (fn-name (string->symbol (string-append "pm-is-"
                                                     (symbol->string note)))))
        (cons (cons (car notes) fn-name)
              (note-predicates (cdr notes))))))

; returns the note <-> note-predicate pair for note, if
;  there is one
; otherwise, returns #f
(define (pm-note-predicate-get-pair note)
  (assoc note *note-predicates*))


; returns the note-predicate for note, if there is one
; otherwise, returns #f
(define (pm-note-predicate-get-predicate note)
  (let ((pair (pm-note-predicate-get-pair note)))
    (if pair
        (cdr pair)
        #f)))


; *notes* is a list of basic notes (define-note-predicates returns the list of
;  notes that is passed to it, not a list of note-predicates)
(define *notes* (define-note-predicates (c c# d d# e f f# g g# a a# b)))


; *note-predicates* is an alist of note <-> note-predicate pairs
(define *note-predicates* (note-predicates *notes*))


; indicates match failure
(define pm-fail #f)


; indicates match success, with no variables
(define pm-no-bindings '((#t . #t)))


; find a (variable . value) pair in a binding list
(define (pm-get-binding var bindings)
  (assoc var bindings))


; get the value part of a single binding
(define (pm-binding-val binding)
  (cdr binding))


; get the value part (for var) from a binding list
(define (pm-lookup var bindings)
  (pm-binding-val (pm-get-binding var bindings)))


; add a (var . value) pair to a binding list
(define (pm-extend-bindings var val bindings)
  (cons (cons var val)
        ; once we get a "real" binding, we can
        ;  get rid of the dummy pm-no-bindings
        (if (eq? bindings pm-no-bindings)
            '()
            bindings)))


; is var a note variables (a variable of the form
;  ?<name>=<note>, where <note> is a member of *notes*)
; eg ?x=c, ?y=a#, ?note=f#
(define (pm-note-variable? var)
  (and (pm-variable? var)
       (let* ((var-name         (symbol->string var))
              (equals-positions (pregexp-match-positions "=" var-name)))
         (and equals-positions
              (let* ((equals-position    (car equals-positions))
                     (equals-index-start (car equals-position))
                     (equals-index-end   (cdr equals-position)))
                (and (> equals-index-start 1)
                     (let ((note (substring var-name
                                            equals-index-end
                                            (string-length var-name))))
                       (member (string->symbol note) *notes*))))))))


; returns the predicate function for testing if a given MIDI pitch
;  is for the same note as var
; note we assume that there var is a valid note-variable
;  (ie we don't perform all the tests pm-note-variable? performs)
; eg (pm-note-variable-predicate '?note=f#) => pm-is-f#
(define (pm-note-variable-predicate var)
  (let* ((var-name           (symbol->string var))
         (equals-positions   (pregexp-match-positions "=" var-name))
         (equals-position    (car equals-positions))
         (equals-index-start (car equals-position))
         (equals-index-end   (cdr equals-position))
         (note               (substring var-name equals-index-end (string-length var-name))))
    (pm-note-predicate-get-predicate (string->symbol note))))
  

; does var match input? uses (or updates) and returns bindings
(define (pm-match-note-variable var input bindings)
  (define (match-note-variable var input)
    (let ((fn (pm-note-variable-predicate var)))
      (if fn
          ((eval fn (interaction-environment)) input)
          #f)))
  (let ((binding (pm-get-binding var bindings)))
    (cond ((not binding)
           (if (match-note-variable var input)
               (pm-extend-bindings var input bindings)
               pm-fail))
          ((equal? input (pm-binding-val binding)) bindings)
          (else pm-fail))))


; is x a variable (a symbol beginning with '?')?
(define (pm-variable? x)
  (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\?)))


; does var match input? uses (or updates) and returns bindings
(define (pm-match-variable var input bindings)
  (let ((binding (pm-get-binding var bindings)))
    (cond ((not binding) (pm-extend-bindings var input bindings))
          ((equal? input (pm-binding-val binding)) bindings)
          (else pm-fail))))


; match the segment pattern ((?* var) . pat) against input
; note that we simulate an optional argument (start), whose
;  default value is 0
(define (pm-segment-match pattern input bindings . start)
  (imp-optional-arg start 0)
  (let ((var (pm-second (pm-first pattern)))
        (pat (pm-rest pattern)))
    (if (null? pat)
        (if (pm-note-variable? var)
            (pm-match-note-variable var input bindings)
            (pm-match-variable var input bindings))
        (let ((pos (pm-first-match-pos (pm-first pat) input start)))
          (if (null? pos)
              pm-fail
              (let ((b2 (pm-match pat
                                  (list-tail input pos)
                                  (if (pm-note-variable? var)
                                      (pm-match-note-variable var
                                                              (pm-subseq input 0 pos)
                                                              bindings)
                                      (pm-match-variable var
                                                         (pm-subseq input 0 pos)
                                                         bindings)))))
                ; if this match failed, try another longer one
                (if (eq? b2 pm-fail)
                    (pm-segment-match pattern input bindings (+ pos 1))
                    b2)))))))


; find the first position that pat1 could possibly match input
;  starting at position start. if pat1 is non-constant, then
;  just return start
(define (pm-first-match-pos pat1 input start)
  (cond ((and (pm-atom? pat1) (not (pm-variable? pat1)))
         (pm-position pat1 input start))
        ((< start (length input))
         start)
        (else pm-nil)))


; is this a segment-matching pattern like ((?* var) . pat)
(define (pm-segment-pattern? pattern)
  (and (pair? pattern) (pair? (pm-first pattern))
       (symbol? (pm-first (pm-first pattern)))
       (pm-segment-match-fn (pm-first (pm-first pattern)))))


; is this a single-matching-pattern?
; eg (?is x predicate)
;    (?and . patterns)
;    (?or . patterns)
(define (pm-single-pattern? pattern)
  (and (pair? pattern)
       (pm-single-match-fn (pm-first pattern))))


; call the right function for this kind of segment pattern
(define (pm-segment-matcher pattern input bindings)
  (let ((fn (pm-segment-match-fn (pm-first (pm-first pattern)))))
    ((eval fn (interaction-environment)) pattern input bindings)))


; call the right function for this kind of single pattern
(define (pm-single-matcher pattern input bindings)
  (let ((fn (pm-single-match-fn (pm-first pattern))))
    ((eval fn (interaction-environment)) (pm-rest pattern) input bindings)))


; get the segment-match function for x
(define (pm-segment-match-fn x)
  (if (symbol? x)
      (let ((pair (assoc x *pm-segment-match-actions*)))
        (if (pair? pair)
            (pm-second pair)
            #f))
      #f))


; get the single-match function for x
(define (pm-single-match-fn x)
  (if (symbol? x)
      (let ((pair (assoc x *pm-single-match-actions*)))
        (if (pair? pair)
            (pm-second pair)
            #f))
      #f))


; succeed and bind var if the input satisfies pred,
; where var-and-pred is the list (var pred)
(define (pm-match-is var-and-pred input bindings)
  (let* ((var (pm-first var-and-pred))
         (pred (pm-second var-and-pred))
         (new-bindings (pm-match var input bindings)))
    (if (or (eq? new-bindings pm-fail)
            (not (apply (eval pred (interaction-environment)) input)))
        pm-fail
        new-bindings)))


; succeed if all the patterns match the input
(define (pm-match-and patterns input bindings)
  (cond ((eq? bindings pm-fail) pm-fail)
        ((null? patterns) bindings)
        (else (match-and (pm-rest patterns)
                         input
                         (pm-match (pm-first patterns)
                                   input
                                   bindings)))))


; succeed if any one of the patterns match the input
(define (pm-match-or patterns input bindings)
  (if (null patterns)
      pm-fail
      (let ((new-bindings (pm-match (pm-first patterns)
                                    input
                                    bindings)))
        (if (eq? new-bindings pm-fail)
            (match-or (pm-rest patterns) input bindings)
            (new-bindings)))))


; succeed if none of the patterns match the input
; this will never bind any variables
(define (pm-match-not patterns input bindings)
  (if (match-or patterns input bindings)
      pm-fail
      bindings))


; match one or more elements of input
(define (pm-segment-match+ pattern input bindings)
  (pm-segment-match pattern input bindings 1))


; match zero or one element of input
(define (pm-segment-match? pattern input bindings)
  (let ((var (pm-second (pm-first pattern)))
        (pat (pm-rest pattern)))
    (or (pm-match (cons var pat) input bindings)
        (pm-match pat input bindings))))


; test an arbitrary expression involving variables
; the pattern looks like ((if code) . rest)
(define (pm-match-if pattern input bindings)
  (and (pm-progv (map car
                      bindings)
                 (map (lambda (binding)
                        (let ((value (cdr binding)))
                          (if (list? value)
                              (list 'quote  value)
                              value)))
                      bindings)
                 (pm-second (pm-first pattern)))
       (pm-match (pm-rest pattern) input bindings)))


; segment pattern-action alist
(define *pm-segment-match-actions*
  '((?*  pm-segment-match)
    (?+  pm-segment-match+)
    (??  pm-segment-match?)
    (?if pm-match-if)))


; single pattern-action alist
(define *pm-single-match-actions*
  '((?is  pm-match-is)
    (?or  pm-match-or)
    (?and pm-match-and)
    (?not pm-match-not)))


; ports of some common CL functions
; used by Impromptu's pattern-matching library


; atom? returns true for anything other than a non-empty list
(define (pm-atom? x)
  (not (and (list? x)
            (> (length x) 0))))


(define (pm-first l)
  (if (null? l)
      '()
      (car l)))


(define (pm-second l)
  (pm-first (pm-rest l)))


(define (pm-rest l)
  (if (null? l)
      '()
      (cdr l)))


(define pm-nil '())


(define (pm-sublis env exp)
  (cond ((null? exp) '())
        ((pair? exp) (cons (pm-sublis env (pm-first exp))
                           (pm-sublis env (pm-rest exp))))
        ((assoc exp env) => cdr)
        (else exp)))


(define (pm-position x l start)
  ; skip skips forward count entries
  ; assumes l has at least count entries
  (define (skip l count)
    (if (= count 0)
        l
        (skip (cdr l) (- count 1))))
  ; simple-pm-position returns the first index of x in l
  ; or pm-nil if none is found
  (define (simple-pm-position x l)
    (cond ((equal? l '()) pm-nil)
          ((equal? x (pm-first l)) 0)
          (else (let ((pos (simple-pm-position x (pm-rest l))))
                  (if (null? pos)
                      pm-nil
                      (+ pos 1))))))
  ; we return pm-nil if l is empty or start >= (length l)
  ; otherwise we skip forward start entries
  ; and use simple-pm-position to do our searching
  (cond ((equal? l '()) pm-nil)
        ((>= start (length l)) pm-nil)
        (else (let* ((l (skip l start))
                     (pos (simple-pm-position x l)))
                (if (null? pos)
                    pm-nil
                    (+ start pos))))))


(define (pm-starts-with? l x)
  (and (list? l)
       (equal? (pm-first l) x)))


; note that we simulate an optional argument (end), whose
;  default value is (length l)
(define (pm-subseq l start . end)
  (define (pm-list-first l end)
    (if (= end 0)
        '()
        (cons (pm-first l)
              (pm-list-first (pm-rest l)
                             (- end 1)))))
  (imp-optional-arg end (length l))
  (if (> end (length l))
      (error "end (" end ") exceeds list length (" (length l) ")"))
  (if (>= start end)
      '()
      (pm-list-first (list-tail l start) (- end start))))


; returns #t if every element of l
;  when applied to fn is true
; returns #t for the empty list
(define (pm-every fn l)
  (cond ((= (length l) 0) #t)
        (else (and (fn (car l))
                   (pm-every fn (cdr l))))))


; returns #t if at least one element of l
;  when applied to fn is true
; ie returns #f for the empty list
(define (pm-some fn l)
  (cond ((= (length l) 0) #f)
        (else (or (fn (car l))
                  (pm-some fn (cdr l))))))


; used by pm-progv macro to create an expression of the form
;
; (let (<binding-list)
;    <expression>)
;
; where <binding-list> is of the form (<variable1> <value1>) (<variable2> <value2>) ...
;       <expression> is a Scheme expression
(define (-pm-progv variables values expression)
  (let ((l (map (lambda (a b) (list a b))
                variables
                values)))
    (list 'let l expression)))


; simple *similar* version of CL's progv
(define-macro (pm-progv variables values expression)
  `(eval (-pm-progv ,variables ,values ,expression) (interaction-environment)))


