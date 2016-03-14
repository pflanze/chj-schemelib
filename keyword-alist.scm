;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; alist parametrized for keywords, and tests

(require easy
	 test
	 (cj-alist keyword-equal?) ;; should we move this?
	 (alist <alist>))

(modimport/prefix keyword-alist:
		  (<alist> keyword? car keyword-equal?))

(TEST
 > (keyword-alist:set '((a: . 0)) '(foo: . 1))
 ((foo: . 1) (a: . 0))
 > (def t '((a: . 1)
	    (b: . 2)
	    (foo: . 3)
	    (bar: . 4)))
 > (keyword-alist:set t '(foo: . 1))
 ((a: . 1)
  (b: . 2)
  (foo: . 1)
  (bar: . 4))
 > (keyword-alist:ref t foo:)
 (foo: . 3))

(TEST
 > (keyword-alist:delete t foo:)
 ((a: . 1) (b: . 2) (bar: . 4))
 > (keyword-alist:delete t baz:)
 ((a: . 1) (b: . 2) (foo: . 3) (bar: . 4))
 > (eq? (keyword-alist:delete t baz:) t)
 #t ;; wow, srfi-1's remove actually cares about that?
 )

