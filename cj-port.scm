;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         srfi-11
         cj-typed
         (cj-env when)
	 test)

(export port-set-unescaped!
        with-output-to-string
	with-error-to-string
	(macro %with-output-to-string)
	(macro %with-error-to-string)
	pretty-string
        port-name)


(define (port-set-unescaped! port)
  (output-port-readtable-set!
   port                                                 
   (readtable-max-unescaped-char-set (output-port-readtable port)
                                     #\U0010ffff)))


(define (make-with-_-to-string current-_-port unescaped?)
  (lambda (thunk)
    (let* ((res 'make-with-_-to-string-unbound)
           (str (call-with-output-string
                 ""
                 (lambda (port)
                   (when unescaped?
                     (port-set-unescaped! port))
                   (set! res
                         (parameterize ((current-_-port port))
                                       (thunk)))))))
      (values str res))))

(define with-output-to-string
  (make-with-_-to-string current-output-port #t))

(define with-error-to-string
  (make-with-_-to-string current-error-port #t))


(define-macro* (%with-output-to-string expr)
  `(with-output-to-string (lambda ()
			    ,expr)))

(define-macro* (%with-error-to-string expr)
  `(with-error-to-string (lambda ()
                           ,expr)))

(TEST
 > (values->vector
    (%with-output-to-string (begin (display "Hello ") (display "World") 10)))
 ["Hello World" 10])


(define (pretty-string v)
  (fst (with-output-to-string (lambda ()
                                (pretty-print v)))))


(TEST
 > (pretty-string (string->symbol "vorwärts"))
 "vorwärts\n")


;; should that be in gambit.scm or something?
(define-typed (port-name [port? v])
  (##port-name v))
