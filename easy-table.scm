(require easy
	 (dsssl sequential-pairs))

"Just some experimenting, what 'literal' 'hashtable' (untyped object
bucket) syntax should/could we have?"


;; |table| is already taken by table.scm, currently; could do
;; renamings of course. But not even sure what I want. Could also
;; implement a new data type, with perfect hashing via macro phase.
(defmacro (TABLE . args)
  `(list->table
    (list ,@(sequential-pairs args
			      (lambda (key val)
				(assert* keyword? key
					 (lambda (key)
					   `(cons ,(keyword->string key)
						  ,val))))))))


(TEST
 > (show (TABLE foo: 1 bar: (+ 39 1)))
 (table (cons "bar" 40) (cons "foo" 1))
 )
