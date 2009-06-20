;;; This file loads init.scm. Put this file in
;;; ~/Library/Application Support/Impromptu so it will be loaded automatically.
;;; Better yet: put a link to it there so you can change its contents here and
;;; don't have to worry about copying it there.
;;;
;;; WARNING: it looks like this file is loaded twice by Impromptu: once when
;;; loading the primary process and once when loading the utility process.
;;; Take care that your init.scm file doesn't care if it's loaded multiple
;;; times.

;; The root directory of this repository on my system. I can't figure out
;; how to get any environment variables, or I'd use HOME instead of this
;; hardcoded value.
(define *my-dir* "/Users/jimm/src/impromptu/")
(load (string-append *my-dir* "init.scm"))
