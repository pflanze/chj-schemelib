;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star)

(export (macro inc)
	(macro dec))


;; also see inc-function and dec-function in cj-env-1--include.scm
;;   e.g.
;; (define (inc-function n)
;;   (declare (fixnum))
;;   (+ n 1))

(define-macro* (inc e)
  `(let ()
     (declare (fixnum))
     (+ ,e 1)))

(define-macro* (dec e)
  `(let ()
     (declare (fixnum))
     (- ,e 1)))


