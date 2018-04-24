;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star)

(export (macro inc)
	(macro dec))


;; Also see safe-fx.scm ! (Perhaps merge?)

;; also see inc-function and dec-function in cj-env-1--include.scm
;;   e.g.
;; (define (inc-function n)
;;   (declare (fixnum))
;;   (+ n 1))


;; In interpreted code, the fixnum declaration is ignored thus the
;; code doesn't report an error for non-fixnums, or fixnum overflows,
;; especially the latter of which is potentially troublesome.

;; Could just rely on fx+ and fx-, but it appeared that declare fixnum
;; with generic ops is faster? TODO: double check.

(define-macro* (inc e)
  (if (mod:compiled?)
      `(let ()
	 (declare (fixnum))
	 (+ ,e 1))
      `(fx+ ,e 1)))

(define-macro* (dec e)
  (if (mod:compiled?)
      `(let ()
	 (declare (fixnum))
	 (- ,e 1))
      `(fx- ,e 1)))


