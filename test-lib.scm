;;; Copyright (<)2011-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Currently just add .show for %try-error results; should move to a
;; proper structure perhaps, too, though.

;; Might want to add cj-test, too (for %try), at some point:

(require define-macro-star
	 test-lib-1
	 show
	 test)

(export try-error-error?
	(method try-error-error.show
                location-error.show
                location.show)
        position-show
        (macro TRY)
	#!optional
	(macro %error))


(define (try-error-error? v)
  (and (vector? v)
       (>= (vector-length v) 2)
       (eq? (vector-ref v 0) 'error)))

(define. (try-error-error.show v show)
  `(%error ,@(map show (cdr (vector->list v)))))

;; XX merge with error-exception.show (actually better approach
;; here--except, now, actually, going to error values anyway and
;; making that uniform)
(define-macro* (%error . args)
  `(%try-error (error ,@args)))


(TEST
 > (define e (%try-error (error "fun")))
 > (try-error-error? e)
 #t
 > (show e)
 (%error "fun")
 > (define e (%try-error (error "fun" (vector 1))))
 > (show e)
 (%error "fun" (vector 1))
 > (equal? (eval #) e)
 #t)


(define (_TRY thunk)
  (continuation-capture
   (lambda (outside)
     (with-exception-catcher
      show
      (lambda ()
        (let ((val (thunk)))
          (continuation-graft outside
                              error "non-exception value:" val)))))))

(define-macro* (TRY expr)
  `(_TRY (lambda () ,expr)))


;; right place?
(define. (location-error.show v show)
  `(location-error ,(show (location-error-location v))
                   ,(show (location-error-message v))
                   ,(show (location-error-args v))))

;; hmm somewhat risky
(define. (location.show v show)
  `(location ,(show (location-container v))
             ,(position-show (location-position v))))

;; don't want to define position.show, position? is just fixnum?
(define (position-show v)
  `(position ,(position-line v)
             ,(position-column v)))

