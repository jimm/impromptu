;;; This file loads init.scm. Put this file in
;;; ~/Library/Application Support/Impromptu so it will be loaded automatically.
;;; Better yet: put a link to it there so you can change its contents here and
;;; don't have to worry about copying it there.
;;;
;;; NOTE: this file is loaded twice by Impromptu: once when loading the
;;; primary process and once when loading the utility process.

;; *my-dir* is the root directory of this repository on my system. It should
;; end with a slash.
;;
;; Instead of hard-coding this path here I'd rather read this value from an
;; environment. Unfortunately very few env vars are defined inside
;; Impromptu. For example, HOME is defined but none of the ones defined in
;; my .bashrc are defined. Thus the hard-coded value.
;;
;; By the way, here's how to read the environment variable HOME:
;;
;; (let* ((proc-info (objc:call "NSProcessInfo" "processInfo"))
;;        (env-dict (objc:call proc-info "environment"))
;;        (env-var (objc:call env-dict "objectForKey:" "HOME")))
;;    (objc:nsstring->string env-var))

(define *my-dir* "/Users/jimm/src/impromptu/")

;; This method loads a file in *my-dir*. The ".scm" extension gets added
;; automatically.
(define load-my-file (lambda (f) (load (string-append *my-dir* f ".scm"))))

(load-my-file "init")
