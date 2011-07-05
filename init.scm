;;; This file is loaded by bootstrap.scm when Impromptu loads.
;;; See the comment there.

(print "Hello, World!")
(macro (comment args) ())               ; (comment (ignored stuff))

(define required-files ())

(define require
  (lambda args
    (for-each
     (lambda (arg)
       (if (member arg required-files)
           #t
           (begin
             (load-my-file arg)
             (set! required-files (cons arg required-files)))))
     args)))
