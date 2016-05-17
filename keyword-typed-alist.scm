;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; alist parametrized for keywords, and tests

;; Mostly COPY of keyword-alist, gah. Mostly needed for the tests.

(require easy
	 test
	 (cj-alist keyword-equal?) ;; should we move this?
	 (typed-alist <typed-alist>))

(modimport/prefix keyword-typed-alist:
		  (<typed-alist> keyword? car keyword-equal?
				 (pair-of keyword? any?)))

(TEST
 > (.list (keyword-typed-alist:set (keyword-typed-alist:alist '(a: . 0))
				   '(foo: . 1)))
 ((foo: . 1) (a: . 0))
 > (def t (keyword-typed-alist:alist '(a: . 1)
				     '(b: . 2)
				     '(foo: . 3)
				     '(bar: . 4)))
 > (.list (keyword-typed-alist:set t '(foo: . 1)))
 ((a: . 1)
  (b: . 2)
  (foo: . 1)
  (bar: . 4))
 > (keyword-typed-alist:ref t foo:)
 (foo: . 3))

(TEST
 > (.list (keyword-typed-alist:delete t foo:))
 ((a: . 1) (b: . 2) (bar: . 4))
 > (.list (keyword-typed-alist:delete t baz:))
 ((a: . 1) (b: . 2) (foo: . 3) (bar: . 4))
 > (eq? (keyword-typed-alist:delete t baz:) t)
 #t ;; wow, srfi-1's remove actually cares about that?
 )

