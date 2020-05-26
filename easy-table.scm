(require easy
         (dsssl sequential-pairs)
	 (table-1 table-of-key)
         (slib-sort sort))

(export (macro TABLE)
        TABLE?
        (method TABLE.show))

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



(def TABLE? (table-of-key string?))
;; XXX check options, too!

(def. (TABLE.show v show)
  `(TABLE ,@(=> (table->list v)
                (sort (on car string>?))
                (ilist.fold
                 (lambda (k+v r)
                   (cons* (string->keyword (car k+v))
                          (show (cdr k+v))
                          r))
                 '()))))

(TEST
 > (def t (TABLE a: 1 c: 2 b: (+ 1 2)))
 > (table-ref t "a")
 1
 > (show t)
 (TABLE a: 1 b: 3 c: 2)
 > (table-set! t d: 4)
 > (show t)
 (table (cons "a" 1) (cons "b" 3) (cons "c" 2) (cons d: 4)))

