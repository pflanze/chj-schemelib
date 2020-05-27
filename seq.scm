;;; Copyright 2019-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-functional-2 either)
         (lazy FV)
         (debuggable-promise possibly-use-debuggable-promise);; ever hack
         (scheme-meta homogenous-vector?)
         (cj-struct-tag cj-struct#vector?)
         (predicates-1 pair-or-null?)
         ;; test
         )

(export iseq?
        iseq-of
        iseq+-of
        seq?
        seq-of

        char-iseq+?)

"Sequence interface definition."
;; XX "Note that unlike Clojure's seq which are called iseq? here, these include (number and other) vectors."


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


(define (iseq? v)
  (FV (v)
      (pair-or-null? v)))

(define (iseq-of pred)
  (lambda (v)
    (FV (v)
	(if (pair? v)
	    (pred (car v))
	    (null? v)))))



(define seq#homogenous-vector?
  (let ()
    (include "scheme-meta-homogenous-vector--include.scm")
    homogenous-vector?))


(define seq? (either iseq?
                     cj-struct#vector?
                     seq#homogenous-vector?))


;;XX what is this again, for?
(define (iseq+-of pred)
  (lambda (v)
    (FV (v)
	(and (pair? v)
             (pred (car v))))))


(define char-iseq+? (iseq+-of char?))

