

;; sick this, forever? cache vs. table.

(require easy
	 fstable
	 jclass)

(jclass ((fscache _fscache)
	 #(fstable? table)
	 #(function? args->string)
	 #(function? args->value) ;; applied
	 #(function? value->string)
	 #(function? string->value)
	 )

	(def (fscache basedir
		      args->string
		      args->value
		      value->string
		      string->value)
	     (_fscache (fstable basedir)
		       args->string
		       args->value
		       value->string
		       string->value))

	(def-method (call* s)
	  (let-fscache
	   ((table
	     args->string
	     args->value
	     value->string
	     string->value) s)
	   
	   (lambda vals
	     (let ((key (args->string vals)))
	       (cond ((fstable.ref table key #f)
		      => string->value)
		     (else
		      (let ((val (apply args->value vals)))
			(fstable.set! table key (value->string val))
			val)))))))

	(def-method (delete* s)
	  (let-fscache
	   ((table
	     args->string
	     args->value
	     value->string
	     string->value) s)
	   
	   (lambda vals
	     (let ((key (args->string vals)))
	       (fstable.possibly-delete! table key))))))

(TEST
 > (%try (create-directory ".test-fscache-dir/")) ;;XX should be part of it, please
 > (def count 0)
 > (def (slow-mult a b)
	;; (thread-sleep! 1)
	(inc! count)
	(* a b))
 > (def c (fscache ".test-fscache-dir"
		   object->string
		   slow-mult
		   ;; XX and these should be derived from types, of
		   ;; course.... forever.
		   number->string
		   string->number))
 > (def cached-mult (.call* c))
 > (def (cached-square x)
	(cached-mult x x))
 > (map cached-square '(2 3 4 5))
 (4 9 16 25)
 > (map cached-square '(2 3 4 5 -2))
 (4 9 16 25 4)
 > count
 5

 ;; > (xsystem "trash" ".efwucn")
 ;; or:
 > (def cached-mult-delete (.delete* c))
 > (def (cached-square-delete x) (cached-mult-delete x x))
 > (map cached-square-delete '(2 3 4 5 -2 8))
 (#t #t #t #t #t #f)
 )
