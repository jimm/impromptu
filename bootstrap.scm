;;; This file loads init.scm. Put this file in
;;; ~/Library/Application Support/Impromptu so it will be loaded automatically.
;;; Better yet: put a link to it there so you can change its contents here and
;;; don't have to worry about copying it there.
;;;
;;; NOTE: this file is loaded twice by Impromptu: once when loading the
;;; primary process and once when loading the utility process.

;; The root directory of this repository on my system. I can't figure out
;; how to get any environment variables, or I'd use HOME instead of this
;; hardcoded value.
(define *my-dir* "/Users/jimm/src/impromptu/")

(define load-my-file (lambda (f) (load (string-append *my-dir* f ".scm"))))

(load-my-file "init")
