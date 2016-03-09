(require easy
	 Maybe
	 (list-util let-pair)
	 (lazy FV)
	 (cj-env-2 C)
	 test)


;; only 1-ary for now
(def (stream-mapfilter/tail fn tail l)
     (delay
       (FV (l)
	   (if (null? l)
	       tail
	       (let-pair ((a l*) l)
			 (let ((rest (& (stream-mapfilter/tail fn tail l*))))
			   (Maybe:cond ((fn a) =>
					(lambda (v)
					  (cons v
						(rest))))
				       (else
					(rest)))))))))


;; only 1-ary for now
(def (stream-mapfilter fn l)
     (stream-mapfilter/tail fn '() l))


(TEST
 > (def s (stream-mapfilter
	   (lambda (v) (if (even? v) (Just v) (Nothing))) '(1 3 4 -2 -1)))
 > (promise? s)
 #t
 > (stream->list s)
 (4 -2))

