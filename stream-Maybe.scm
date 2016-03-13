(require easy
	 Maybe
	 (list-util let-pair)
	 (lazy FV)
	 (cj-env-2 C)
	 test)

(export mapfilter/tail
	stream-mapfilter/tail
	mapfilter
	stream-mapfilter)


;; only 1-ary for now
(define-strict-and-lazy
  mapfilter/tail
  stream-mapfilter/tail
  (named self
	 (lambda (fn tail l)
	   (DELAY
	    (FV (l)
		(if (null? l)
		    tail
		    (let-pair ((a l*) l)
			      (let ((rest (& (self fn tail l*))))
				(Maybe:cond ((fn a) =>
					     (lambda (v)
					       (cons v
						     (rest))))
					    (else
					     (rest)))))))))))


;; only 1-ary for now
(def (stream-mapfilter fn l)
     (stream-mapfilter/tail fn '() l))

(def (mapfilter fn l)
     (mapfilter/tail fn '() l))


(TEST
 > (def s (stream-mapfilter
	   (lambda (v) (if (even? v) (Just v) (Nothing))) '(1 3 4 -2 -1)))
 > (promise? s)
 #t
 > (stream->list s)
 (4 -2)
 > (mapfilter (lambda (v) (if (even? v) (Just v) (Nothing))) '(1 3 4 -2 -1))
 (4 -2))

