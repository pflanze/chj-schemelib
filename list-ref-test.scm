
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
 > (keyword-alist:set '((a: . 1)
			(b: . 2)
			(foo: . 3)
			(bar: . 4))
		      '(foo: . 1))
 ((a: . 1)
  (b: . 2)
  (foo: . 1)
  (bar: . 4)))

