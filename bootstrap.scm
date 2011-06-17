;;; This file loads init.scm. Put this file in
;;; ~/Library/Application Support/Impromptu so it will be loaded automatically.
;;; Better yet: put a link to it there so you can change its contents here and
;;; don't have to worry about copying it there.
;;;
;;; NOTE: this file is loaded twice by Impromptu: once when loading the
;;; primary process and once when loading the utility process.

;; The root directory of this repository on my system. I've figured out how
;; to read environment variables, but very few are defined inside Imprompt.
;; For example, HOME is defined byt none of the ones defined in my .bashrc
;; are defined. Thus the hard-coded value.
(define *my-dir* "/Users/jimm/src/impromptu/")

(define load-my-file (lambda (f) (load (string-append *my-dir* f ".scm"))))

(load-my-file "init")
