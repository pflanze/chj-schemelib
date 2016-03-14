
;; Tests for list-ref.scm

(require easy
	 test
	 (cj-alist keyword-equal?) ;; just for testing
	 (list-ref <list-ref>)
	 )

(modimport/prefix keyword-alist:
		  (<list-ref> keyword? car keyword-equal?))

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

