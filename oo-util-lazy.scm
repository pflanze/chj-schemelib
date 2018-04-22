;;; Copyright 2017-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-1
	 dot-oo ;; included in easy?
	 ;; oo-util ;; ?
	 (cj-source-quasiquote quasiquote-source)
	 stream ;; (only optionally? (lazily? well.))
	 stream-Maybe ;; (ah well, more dependencies)
	 list-util-1
	 (template template-map)
	 (oo-vector-lib sum)
	 debuggable-promise
	 (srfi-1 null-list?)
	 )

(export ilist?
	istream?
	ilist-of
	istream-of
	ivector?
	ivector-of
	char-ilist?
	char-istream?
	char-ivector?
	source-char-ilist?
	source-char-istream?
	possibly-lazy-null?
	lazy-null?
	evaluated-strictly-stream?
	evaluated-stream? evaluated-strictly-stream-of
	evaluated-stream-of
	evaluated-stream+-of
	evaluated-char-stream+?
	(method evaluated-char-stream+.show)
	iseq?
	iseq-of
	iseq+of
	char-iseq+?
	;; and then many methods and list function aliases ...
	)

(possibly-use-debuggable-promise)

;; Methods on lazy data

(insert-result-of
 `(begin
    ,@(map (lambda (method-name)
	     (quasiquote-source
	      (define. (,(source:symbol-append 'promise method-name) v)
		(,method-name (force v)))))
	   '(
	     .length ;; vs. istream.length which only works on streams
	     .car
	     .cdr
	     .first
	     .rest
	     ;; .cadr
	     ;; .cddr  ah won't work, need to dispatch to deeper-forcing ones
	     ;; .ref ;; hmm there is a list.ref, will be slow
	     ;;.equal nope, instead istream.equal? can work
	     ))))



;; Types: don't want to use list?, as that's inefficient and for
;; stream even worse. Thus don't want to name it stream? for
;; confusion, also, list.foo is bad since it both suggests it does
;; something it doesn't do and can't actually without redefinining
;; list?, hence i prefix, OK? For insecure, immediate, or so.

(define (ilist? v)
  (or (null? v)
      (and (pair? v)
	   (pair-or-null? (cdr v)))))

(define (istream? v)
  ;; *only* for lazy inputs
  (or (and (promise? v)
	   (pair-or-null? (force v)))
      (and (pair? v)
	   (let ((r (cdr v)))
	     (and (promise? r)
		  (pair-or-null? (force r)))))))

(TEST
 > (istream? (delay (cons 1 (delay '()))))
 #t
 > (istream? (cons 1 (delay '())))
 #t
 > (istream? (cons* 1 2 (delay '())))
 #f)

(define (ilist-of pred)
  (lambda (v)
    (or (null? v)
	(and (pair? v)
	     (let-pair ((a r) v)
		       (and (pred a)
			    (pair-or-null? r)))))))

(define (istream-of pred)
  (lambda (v)
    ;; *only* for lazy inputs
    (let ((check (lambda (v)
		   (FV (v)
		       (or (null? v)
			   (and (pair? v)
				(pred (car v))))))))
      (or (and (promise? v)
	       (check v))
	  (and (pair? v)
	       (pred (car v))
	       (let ((r (cdr v)))
		 (and (promise? r)
		      (check r))))))))

(define ivector? vector?)

(define (ivector-of pred)
  (lambda (v)
    (and (vector? v)
	 (let ((len (vector-length v)))
	   (or (zero? len)
	       (pred (vector-ref v 0)))))))

(define char-ilist? (ilist-of char?))
(define char-istream? (istream-of char?))
(define char-ivector? (ivector-of char?))

(TEST
 > (char-istream? '())
 #f ;; because there's *no* indication of lazyness
 > (char-istream? (delay '()))
 #t
 > (char-ilist? '())
 #t
 > (char-istream? '(#\H #\i))
 #f
 > (char-ilist? '(#\H #\i))
 #t
 > (char-istream? '(#\H 1))
 #f
 > (char-ilist? '(#\H 1))
 #t
 ;; ^ well, decided that this is good enough.

 > (char-ilist? (delay (cons #\H (delay (cons #\i '())))))
 #f
 > (char-istream? (delay (cons #\H (delay (cons #\i '())))))
 #t
 > (char-ilist? (cons #\H (delay (cons #\i '()))))
 #f
 > (char-istream? (cons #\H (delay (cons #\i '()))))
 #t

 > (char-ilist? (delay (cons #\H (delay 1))))
 #f
 > (char-istream? (delay (cons #\H (delay 1))))
 #t

 > (char-ilist? '(1 #\H))
 #f
 > (char-istream? (delay (cons 1 (delay (cons #\H '())))))
 #f

 > (char-istream? (cons #\H (delay '())))
 #t
 > (char-istream? (cons 'H (delay '())))
 #f
 > (char-istream? (delay (cons 'H (delay '()))))
 #f
 > (char-istream? (delay (cons #\H '())))
 #t
 )


(define source-char-ilist? (ilist-of (source-of char?)))
(define source-char-istream? (istream-of (source-of char?)))


(define (possibly-lazy-null? v)
  (or (null? v)
      (and (promise? v)
	   (null? (force v)))))

(define (lazy-null? v)
  (and (promise? v)
       (null? (force v))))



;; fully evaluated streams

(define (evaluated-strictly-stream? v)
  (and (evaluated-promise? v)
       (let ((v (evaluated-promise-value v)))
	 (or (null? v)
	     (and (pair? v)
		  (evaluated-strictly-stream? (cdr v)))))))

;; allow the first cons to be evaluated already like in istream?:
(define (evaluated-stream? v)
  (or (if (promise? v)
	  (evaluated-strictly-stream? v)
	  (and (pair? v)
	       (evaluated-strictly-stream? (cdr v))))))


(TEST
 > (def s (.stream "hallo"))
 > (evaluated-stream? s)
 #f
 > (force s)
 > (evaluated-stream? s)
 #f
 > (evaluated-stream? (force s))
 #f
 > (F (stream-take s 2))
 > (def s2 (S s)) ;; 2 first elements non-lazy
 > (evaluated-stream? s)
 #f
 > (istream? s)
 #t
 > (istream? (force s))
 #t
 > (istream? (S s))
 #f
 > (F s)
 (#\h #\a #\l #\l #\o)
 > (evaluated-stream? s)
 #t
 > (evaluated-stream? (force s))
 #t
 > (evaluated-stream? s2)
 #f ;; since *two* elements at the front are not promises
 )

(define (evaluated-strictly-stream-of pred)
  (named rec
	 (lambda (v)
	   (and (evaluated-promise? v)
		(let ((v (evaluated-promise-value v)))
		  (or (null? v)
		      (and (pair? v)
			   (pred (car v))
			   (rec (cdr v)))))))))

(define (evaluated-stream-of pred)
  (define evaluated-strictly-stream? (evaluated-strictly-stream-of pred))
  (lambda (v)
    (or (if (promise? v)
	    (evaluated-strictly-stream? v)
	    (and (pair? v)
		 (evaluated-strictly-stream? (cdr v)))))))

(define (evaluated-stream+-of pred)
  (define evaluated-stream? (evaluated-stream-of pred))
  (lambda (v)
    (and (evaluated-stream? v)
	 (pair? (force v)))))

(TEST
 > ((evaluated-stream+-of char?) '())
 #f
 > ((evaluated-stream+-of char?) '(#\a))
 #f
 > (def s3 (delay (cons #\a (delay '()))))
 > ((evaluated-stream+-of char?) s3)
 #f
 > (force s3)
 > ((evaluated-stream+-of char?) s3)
 #f
 > (F s3)
 > ((evaluated-stream+-of char?) s3)
 #t
 > ((evaluated-stream+-of char?) (force s3))
 #t
 > ((evaluated-stream-of char?) s3)
 #t
 > ((evaluated-strictly-stream-of char?) (force s3))
 #f
 > ((evaluated-stream+-of boolean?) s3)
 #f
 )


(define evaluated-char-stream+? (evaluated-stream+-of char?))

(define. (evaluated-char-stream+.show v)
  `(.stream ,(stream->string v)))

(TEST
 > (def s4 (.stream "foo"))
 > (.show (F s4))
 (.list "foo")
 > (.show s4)
 (.stream "foo")
 ;; (XX add tests, here and with all .show tests, to verify that
 ;; eval'ing the result is actually leading to an equivalent input)
 )


;; move to an iseq.scm ?

(define (iseq? v)
  (FV (v)
      (pair-or-null? v)))

(define (iseq-of pred)
  (lambda (v)
    (FV (v)
	(if (pair? v)
	    (pred (car v))
	    (null? v)))))


(define (iseq+-of pred)
  (lambda (v)
    (FV (v)
	(if (pair? v)
	    (pred (car v))
	    #f))))


(define char-iseq+? (iseq+-of char?))



;; methods after forcing:

(define. ilist.length improper-length)
;;(define. ilist.list identity) ;; well... dangerous.

(define. pair.car car)
(define. pair.cdr cdr)
(define. pair.first first)
(define. pair.rest rest)
;; (define. pair.)
;; (define. pair.)

(define. list.list identity)
;; ^ but actual forcing of a stream happens by dispatching to
;; stream.list (which should have, and has, priority,
;; hence, this one is only called for non-lazy inputs).


;; lazy-to-lazy methods:

;; XX should we just fall back on one generic method (hey, like in
;; fperl!), that forces then dispatches to the eager method, I mean,
;; then signalizes that it is to return a lazy result? I.e. dispatch
;; to a different namespace?)


;; for easy matching below. Consistency, you know..
(define list-drop drop)
(define list-take take)
(define list-last last)
(define list-butlast butlast)
(define list-sublist sublist)
(define list-length length)
(define list-sum sum)
(define list-append append)
(define list-append/2 append)
;;(define list-xone xone)
(define list-min&max stream-min&max) ;; XX add non-forcing instead? consistency forever? evil now
(define list-min stream-min)
(define list-max stream-max)
(define list-for-each for-each)

(define list-map map)
(define (list-filter/tail pred lis tail)
  (let rec ((l lis))
    (if (pair? l)
	(let-pair ((a l*) l)
		  (If (pred a)
		      (cons a (rec l*))
		      (rec l*)))
	tail)))
;;(define list-filter filter) ah, no
(define (list-filter pred lis #!optional (tail '()))
  (list-filter/tail pred lis tail))

(define list-improper-map improper-map)
(define list-map/tail map/tail)
(define list-map/iota map/iota)
(define list-filter/iota filter/iota)
(define list-fold fold)
(define list-fold-right fold-right)
(define list-fold-right/iota fold-right/iota)
(define list-fold-right/iota+rest fold-right/iota+rest)
(define list-filter-map/iota filter-map/iota)
(define list-filter-map filter-map)

(define list-mapfilter/tail stream-mapfilter/tail)
(define list-mapfilter stream-mapfilter)

(define list-zip zip)
(define list-zip2 zip2)
(define list-find-tail find-tail)
(define list-take-while take-while)
(define list-drop-while drop-while)
(define list-chop chop)
(define list-chop/map chop/map)
(define list-every every)

(define list-reverse reverse)
(define list-reverse/tail reverse/tail)

(define source-list->string source-stream->string) ;; slight inefficiency

(define list-length>= stream-length>=)
(define list-length> stream-length>)

(define list-first first)
(define list-second second)
(define list-third third)
(define list-fourth fourth)
(define list-fifth fifth)
(define list-sixth sixth)
(define list-seventh seventh)
(define list-eighth eighth)
(define list-ninth ninth)
(define list-tenth tenth)

(define list->list identity)

(define list-null? null-list?)
(define list-pair? pair?)

(define list-for-each/iota for-each/iota)


(template-map
 ((istream. '(istream. ilist.))
  (stream- '(stream- list-)))
 (begin

   (define. (istream.null? s)
     (stream-null? s))

   (define. (istream.pair? s)
     (stream-pair? s))

   (define. (istream.filter/tail s pred tail)
     (stream-filter/tail pred s tail))

   (define. (istream.for-each s proc . ss)
     (apply stream-for-each proc s ss))

   (define. (istream.fold-right l fn start)
     (stream-fold-right fn start l))

   ;; stream:fold-right  -- hmm, bad name anyway

   (define. (istream.map/tail s func tail)
     (stream-map/tail func s tail))

   (define. (istream.map s f . ss)
     (apply stream-map f s ss))

   (define. (istream.filter-map s f . ss)
     (apply stream-filter-map f s ss))

   (define. (istream.mapfilter/tail l fn tail)
     (stream-mapfilter/tail fn tail l))

   (define. (istream.mapfilter l fn . rest)
     (apply stream-mapfilter fn l rest))
   
   (define. (istream.improper-map func s)
     (stream-improper-map func s))

   (define. (istream.list s)
     (stream->list s))

   (define. istream.drop
     stream-drop)

   (define. istream.take stream-take)

   (define. istream.last stream-last)

   (define. istream.butlast stream-butlast)

   (define. istream.sublist stream-sublist)

   (define. istream.length stream-length) ;; XX should really be a stream-improper-length !

   (define. istream.difference stream-difference)

   (define. istream.show-difference show-stream-difference)

   (define. istream.equal? stream-equal?)

   (define. (istream.filter s pred #!optional (tail '()))
     (stream-filter pred s tail))

   (define. (istream.fold l fn start)
     (stream-fold fn start l))

   (define. istream.append-optimized stream-append-optimized)

   (define. istream.append/2 stream-append/2)

   (define. istream.append stream-append)
   ;; also see {lazy-,}null.append below

   (define. (istream.union s less? . ss)
     (apply stream-union less? s ss))

   (define. (istream.uniq s equal? #!optional (tail '()))
     (stream-uniq equal? s tail))

   (define. (istream.uniq-count s equal? #!optional (tail '()))
     (stream-uniq-count equal? s tail))

   (define. (istream.cmp-union s cmp . ss)
     (apply cmp-stream-union cmp s ss))

   (define. (istream.group s equal? #!optional (tail '()))
     (stream-group equal? s tail))

   (define. (istream.cmp-group s cmp #!optional (tail '()))
     (cmp-stream-group cmp s tail))

   (define. (istream.chop/map s n f #!optional (tail '()))
     (stream-chop/map n s f tail))

   (define. (istream.chop n s #!optional (tail '()))
     (stream-chop n s tail))

   (define. (istream.zip s . ss)
     (apply stream-zip s ss))

   (define. (istream.zip2 s1 s2)
     (stream-zip2 s1 s2))

   (define. (istream.find-tail l pred)
     (stream-find-tail pred l))

   (define. (istream.take-while l pred)
     (stream-take-while pred l))

   (define. (istream.drop-while l pred)
     (stream-drop-while pred l))

   (define. istream.ref stream-ref)

   (define. istream.Maybe-ref stream-Maybe-ref)

   (define. (istream.every lis1 pred . lists)
     (apply stream-every pred lis1 lists))

   (define. istream.min&max stream-min&max)
   ;; are these evil, since |min| and |max| assume n-ary interfaces?:
   ;; (At least it should be unambiguous though as only work on
   ;; numbers, not lists, as elements.)
   (define. istream.min stream-min)
   (define. istream.max stream-max)

   (define. (istream.map/iota lis fn)
     (stream-map/iota fn lis))

   (define. (istream.filter/iota lis pred)
     (stream-filter/iota pred lis))

   (define. (istream.fold-right/iota s kons tail)
     (stream-fold-right/iota kons tail s))

   (define. (istream.fold-right/iota+rest s func tail)
     (stream-fold-right/iota+rest func tail s))

   (define. (istream.filter-map/iota s func . rest)
     (apply stream-filter-map/iota func s rest))

   (define. (istream.for-each/iota s proc)
     (stream-for-each/iota proc s))

   (define. istream.sum stream-sum)

   (define. istream.rtake&rest stream-rtake&rest)
   (define. istream.reverse stream-reverse)
   (define. istream.reverse/tail stream-reverse/tail)
   (define. istream.split-at stream-split-at)

   (define. char-istream.string stream->string)
   (define. source-char-istream.string source-stream->string)

   (define. istream.length> stream-length>)
   (define. istream.length>= stream-length>=)

   ;; srfi-1

   (define. istream.second stream-second)
   (define. istream.third stream-third)
   (define. istream.fourth stream-fourth)
   (define. istream.fifth stream-fifth)
   (define. istream.sixth stream-sixth)
   (define. istream.seventh stream-seventh)
   (define. istream.eighth stream-eighth)
   (define. istream.ninth stream-ninth)
   (define. istream.tenth stream-tenth)
   ))



(define. (null.append a b . r)
  (if (null? r)
      b
      (if (null? b)
	  (apply append r)
	  (apply append b r))))

(define. (lazy-null.append a b . r)
  (if (null? r)
      b
      (if (possibly-lazy-null? b)
	  (apply stream-append r)
	  (apply stream-append b r))))


(TEST
 > (.first (cons 1 2))
 1
 > (.first (delay (cons 1 2)))
 1
 > (.car (cons 1 2))
 1
 > (.car (delay (cons 1 2)))
 1
 > (.cdr (cons 1 2))
 2
 > (.cdr (delay (cons 1 2)))
 2

 > (.car (.cdr (stream-iota 10)))
 1
 > (.sum (iota 10))
 45
 > (.sum (stream-iota 10))
 45
 > (.sum (.map (iota 10) inc))
 55
 > (.sum (.map (stream-iota 10) inc))
 55
 > (.max '(1 9 4))
 9
 > (.max (stream-iota 10 5))
 14
 > (def l '(a))
 > (eq? (.append '() l) l)
 #t
 > (.append '(b) l)
 (b a)
 > (.list (.append (stream-iota 2) l))
 (0 1 a)
 > (eq? (.append (stream-iota 0) l) l)
 #t
 > (.filter-map/iota '(1 a 3 b) (lambda (v i) (and (even? i) (inc v))))
 (2 4)
 > (F (.filter-map/iota (stream-iota 4) (lambda (v i) (and (even? i) (inc v)))))
 (1 3)

 > (.reverse/tail '(a b c) '(1 2))
 (c b a 1 2)
 > (.reverse '(a b c))
 (c b a)
 > (.list (.rtake&rest '(a b c d) 2))
 ((b a) (c d))
 > (.list (.split-at '(a b c d) 2))
 ((a b) (c d))

 > (.string '(#\H #\i))
 "Hi"
 > (.string (delay (cons #\H (delay (cons #\i '())))))
 "Hi"

 ;; add more extensive testing..
 )


