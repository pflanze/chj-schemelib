;;; Copyright 2014-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require template
	 easy-1
	 test
	 (cj-test %try)
	 char-util)

;; functions not implemented but required by oo-util-lazy if adding
;; vector as type parameter there:
(def (error-not-implemented/n . args)
     (error-not-implemented))
(def vector->string error-not-implemented/n)
(def source-vector->string error-not-implemented/n)
(def vector-length> error-not-implemented/n)
(def vector-length>= error-not-implemented/n)
(def vector-fourth error-not-implemented/n)
(def vector-fifth error-not-implemented/n)
(def vector-sixth error-not-implemented/n)
(def vector-seventh error-not-implemented/n)
(def vector-eighth error-not-implemented/n)
(def vector-ninth error-not-implemented/n)
(def vector-tenth error-not-implemented/n)

(def char-vector? false/1) ;; XX evil
(def source-char-vector? false/1) ;; XX evil


;; This is the great combinatorial explosion file.

(export sum

	string->u8vector string.u8vector ;; XX remove this?
	u8vector.string			 ;; dito?

	;; XX why all the "-" versions instead of just the "." versions?

	string.ref
	string.set!
	string.length
	string.list
	string.null?
	string.first
	string.last
	string.rest
	string.filter/iota
	string.filter
	string.for-each/iota
	;; string-inc!
	;; string-set!
	;; string.inc!
	;; string-dec!
	;; string.dec!
	;; string.sum
	string-map/iota
	string.map/iota
	string.chop-both-ends
	string-for-each
	string.for-each
	string.map
	string.map-list

	strings-map
	string-map
	string-map*
	string-fold-right
	string.fold-right
	string-fold
	string.fold
	strings-append
	pair-with-car-string?
	pair-with-car-string.append
	;; string.reverse is defined in oo-util

	;; the remaining ones have more variants
	vector.ref
	vector.set!
	vector.length
	vector.list
	list.vector
	vector.null?
	vector.first
	vector.last
	vector.rest
	vector.filter/iota
	vector.filter
	vector.for-each/iota
	vector-inc!
	vector-set!
	vector.inc!
	vector-dec!
	vector.dec!
	vector.sum
	vector-map/iota
	vector.map/iota
	vector.chop-both-ends
	vector-for-each
	vector.for-each
	vector.map
	vector.map-list

	vectors-map
	vector-map
	vector-map*
	vector-fold-right
	vector.fold-right
	vector-fold
	vector.fold
	vectors-append
	pair-with-car-vector?
	pair-with-car-vector.append
	vector-reverse vector.reverse

	f32vector.ref
	f32vector.set!
	f32vector.length
	f32vector.list
	list.f32vector
	f32vector.null?
	f32vector.first
	f32vector.last
	f32vector.rest
	f32vector.filter/iota
	f32vector.filter
	f32vector.for-each/iota
	f32vector-inc!
	f32vector-set!
	f32vector.inc!
	f32vector-dec!
	f32vector.dec!
	f32vector.sum
	f32vector-map/iota
	f32vector.map/iota
	f32vector.chop-both-ends
	f32vector-for-each
	f32vector.for-each
	f32vector.map
	f32vector.map-list

	f32vectors-map
	f32vector-map
	f32vector-map*
	f32vector-fold-right
	f32vector.fold-right
	f32vector-fold
	f32vector.fold
	f32vectors-append
	pair-with-car-f32vector?
	pair-with-car-f32vector.append
	f32vector-reverse f32vector.reverse

	f64vector.ref
	f64vector.set!
	f64vector.length
	f64vector.list
	list.f64vector
	f64vector.null?
	f64vector.first
	f64vector.last
	f64vector.rest
	f64vector.filter/iota
	f64vector.filter
	f64vector.for-each/iota
	f64vector-inc!
	f64vector-set!
	f64vector.inc!
	f64vector-dec!
	f64vector.dec!
	f64vector.sum
	f64vector-map/iota
	f64vector.map/iota
	f64vector.chop-both-ends
	f64vector-for-each
	f64vector.for-each
	f64vector.map
	f64vector.map-list

	f64vectors-map
	f64vector-map
	f64vector-map*
	f64vector-fold-right
	f64vector.fold-right
	f64vector-fold
	f64vector.fold
	f64vectors-append
	pair-with-car-f64vector?
	pair-with-car-f64vector.append
	f64vector-reverse f64vector.reverse

	u8vector.ref
	u8vector.set!
	u8vector.length
	u8vector.list
	list.u8vector
	u8vector.null?
	u8vector.first
	u8vector.last
	u8vector.rest
	u8vector.filter/iota
	u8vector.filter
	u8vector.for-each/iota
	u8vector-inc!
	u8vector-set!
	u8vector.inc!
	u8vector-dec!
	u8vector.dec!
	u8vector.sum
	u8vector-map/iota
	u8vector.map/iota
	u8vector.chop-both-ends
	u8vector-for-each
	u8vector.for-each
	u8vector.map
	u8vector.map-list

	u8vectors-map
	u8vector-map
	u8vector-map*
	u8vector-fold-right
	u8vector.fold-right
	u8vector-fold
	u8vector.fold
	u8vectors-append
	pair-with-car-u8vector?
	pair-with-car-u8vector.append
	u8vector-reverse u8vector.reverse

	s8vector.ref
	s8vector.set!
	s8vector.length
	s8vector.list
	list.s8vector
	s8vector.null?
	s8vector.first
	s8vector.last
	s8vector.rest
	s8vector.filter/iota
	s8vector.filter
	s8vector.for-each/iota
	s8vector-inc!
	s8vector-set!
	s8vector.inc!
	s8vector-dec!
	s8vector.dec!
	s8vector.sum
	s8vector-map/iota
	s8vector.map/iota
	s8vector.chop-both-ends
	s8vector-for-each
	s8vector.for-each
	s8vector.map
	s8vector.map-list

	s8vectors-map
	s8vector-map
	s8vector-map*
	s8vector-fold-right
	s8vector.fold-right
	s8vector-fold
	s8vector.fold
	s8vectors-append
	pair-with-car-s8vector?
	pair-with-car-s8vector.append
	s8vector-reverse s8vector.reverse

	u16vector.ref
	u16vector.set!
	u16vector.length
	u16vector.list
	list.u16vector
	u16vector.null?
	u16vector.first
	u16vector.last
	u16vector.rest
	u16vector.filter/iota
	u16vector.filter
	u16vector.for-each/iota
	u16vector-inc!
	u16vector-set!
	u16vector.inc!
	u16vector-dec!
	u16vector.dec!
	u16vector.sum
	u16vector-map/iota
	u16vector.map/iota
	u16vector.chop-both-ends
	u16vector-for-each
	u16vector.for-each
	u16vector.map
	u16vector.map-list

	u16vectors-map
	u16vector-map
	u16vector-map*
	u16vector-fold-right
	u16vector.fold-right
	u16vector-fold
	u16vector.fold
	u16vectors-append
	pair-with-car-u16vector?
	pair-with-car-u16vector.append
	u16vector-reverse u16vector.reverse

	s16vector.ref
	s16vector.set!
	s16vector.length
	s16vector.list
	list.s16vector
	s16vector.null?
	s16vector.first
	s16vector.last
	s16vector.rest
	s16vector.filter/iota
	s16vector.filter
	s16vector.for-each/iota
	s16vector-inc!
	s16vector-set!
	s16vector.inc!
	s16vector-dec!
	s16vector.dec!
	s16vector.sum
	s16vector-map/iota
	s16vector.map/iota
	s16vector.chop-both-ends
	s16vector-for-each
	s16vector.for-each
	s16vector.map
	s16vector.map-list

	s16vectors-map
	s16vector-map
	s16vector-map*
	s16vector-fold-right
	s16vector.fold-right
	s16vector-fold
	s16vector.fold
	s16vectors-append
	pair-with-car-s16vector?
	pair-with-car-s16vector.append
	s16vector-reverse s16vector.reverse

	u32vector.ref
	u32vector.set!
	u32vector.length
	u32vector.list
	list.u32vector
	u32vector.null?
	u32vector.first
	u32vector.last
	u32vector.rest
	u32vector.filter/iota
	u32vector.filter
	u32vector.for-each/iota
	u32vector-inc!
	u32vector-set!
	u32vector.inc!
	u32vector-dec!
	u32vector.dec!
	u32vector.sum
	u32vector-map/iota
	u32vector.map/iota
	u32vector.chop-both-ends
	u32vector-for-each
	u32vector.for-each
	u32vector.map
	u32vector.map-list

	u32vectors-map
	u32vector-map
	u32vector-map*
	u32vector-fold-right
	u32vector.fold-right
	u32vector-fold
	u32vector.fold
	u32vectors-append
	pair-with-car-u32vector?
	pair-with-car-u32vector.append
	u32vector-reverse u32vector.reverse

	s32vector.ref
	s32vector.set!
	s32vector.length
	s32vector.list
	list.s32vector
	s32vector.null?
	s32vector.first
	s32vector.last
	s32vector.rest
	s32vector.filter/iota
	s32vector.filter
	s32vector.for-each/iota
	s32vector-inc!
	s32vector-set!
	s32vector.inc!
	s32vector-dec!
	s32vector.dec!
	s32vector.sum
	s32vector-map/iota
	s32vector.map/iota
	s32vector.chop-both-ends
	s32vector-for-each
	s32vector.for-each
	s32vector.map
	s32vector.map-list

	s32vectors-map
	s32vector-map
	s32vector-map*
	s32vector-fold-right
	s32vector.fold-right
	s32vector-fold
	s32vector.fold
	s32vectors-append
	pair-with-car-s32vector?
	pair-with-car-s32vector.append
	s32vector-reverse s32vector.reverse

	u64vector.ref
	u64vector.set!
	u64vector.length
	u64vector.list
	list.u64vector
	u64vector.null?
	u64vector.first
	u64vector.last
	u64vector.rest
	u64vector.filter/iota
	u64vector.filter
	u64vector.for-each/iota
	u64vector-inc!
	u64vector-set!
	u64vector.inc!
	u64vector-dec!
	u64vector.dec!
	u64vector.sum
	u64vector-map/iota
	u64vector.map/iota
	u64vector.chop-both-ends
	u64vector-for-each
	u64vector.for-each
	u64vector.map
	u64vector.map-list

	u64vectors-map
	u64vector-map
	u64vector-map*
	u64vector-fold-right
	u64vector.fold-right
	u64vector-fold
	u64vector.fold
	u64vectors-append
	pair-with-car-u64vector?
	pair-with-car-u64vector.append
	u64vector-reverse u64vector.reverse

	s64vector.ref
	s64vector.set!
	s64vector.length
	s64vector.list
	list.s64vector
	s64vector.null?
	s64vector.first
	s64vector.last
	s64vector.rest
	s64vector.filter/iota
	s64vector.filter
	s64vector.for-each/iota
	s64vector-inc!
	s64vector-set!
	s64vector.inc!
	s64vector-dec!
	s64vector.dec!
	s64vector.sum
	s64vector-map/iota
	s64vector.map/iota
	s64vector.chop-both-ends
	s64vector-for-each
	s64vector.for-each
	s64vector.map
	s64vector.map-list

	s64vectors-map
	s64vector-map
	s64vector-map*
	s64vector-fold-right
	s64vector.fold-right
	s64vector-fold
	s64vector.fold
	s64vectors-append
	pair-with-car-s64vector?
	pair-with-car-s64vector.append
	s64vector-reverse s64vector.reverse

	)


(def (error-not-implemented)
     (error "not yet implemented"))

(def inc (inline inc))


(def (sum nums)
     (fold + 0 nums))

(template-map
 ((VECTOR '(
	    string
	    vector
	    f32vector
	    f64vector
	    u8vector s8vector
	    u16vector s16vector
	    u32vector s32vector
	    u64vector s64vector)))
 
 (begin
   (def. VECTOR.ref VECTOR-ref)
   (def. VECTOR.set! VECTOR-set!)
   (def. VECTOR.length VECTOR-length)
   (IF (not (eq? 'VECTOR 'vector))
       ;; Don't generate vector.append method as long as structs are
       ;; vectors and there's no hierarchy for dot-oo methods!  XX
       ;; well, same problem for any of the methods if the same name
       ;; is to be used with a struct.
       (def. VECTOR.append VECTOR-append))
   (def. VECTOR.list VECTOR->list)
   (IF (not (eq? 'VECTOR 'string))
       ;; list.string is not OK, use char-list.string from
       ;; oo-util.scm instead.
       (def. list.VECTOR list->VECTOR))

   ;; XX already have |string-empty?|
   (def (VECTOR-null? v)
     (zero? (VECTOR-length v)))
   (def. VECTOR.null? VECTOR-null?)

   (def (VECTOR-filter/iota fn v)
     (let* ((len (VECTOR-length v))
	    (v* (make-VECTOR len)))
       (let lp ((i 0)
		(j 0))
	 (if (fx< i len)
	     (let ((val (VECTOR-ref v i)))
	       (if (fn val i)
		   (begin
		     (VECTOR-set! v* j val)
		     (lp (inc i) (inc j)))
		   (lp (inc i) j)))
	     (begin
	       (VECTOR-shrink! v* j)
	       v*)))))
   (def. (VECTOR.filter/iota v fn)
     (VECTOR-filter/iota fn v))

   (def (VECTOR-filter fn v)
     (VECTOR.filter/iota v (lambda (val i)
			     (fn val))))
   (def. (VECTOR.filter v fn)
     (VECTOR-filter fn v))
   
   ;; XX move to/merge with vector-util.scm: (vector-for-each proc vec) .. ?

   (def. (VECTOR.for-each/iota v proc)
     (let ((len (VECTOR-length v)))
       (for..< (i 0 len)
	       (proc (VECTOR-ref v i)
		     i))))
   
   (IF (not (eq? 'VECTOR 'string))
       (begin
	 ;; Heh these are still using the R5RS number operations
	 ;; generics. Rely on the host system to optimize these.
	 (def (VECTOR-inc! v i)
	      (VECTOR-set! v i
			   (+ (VECTOR-ref v i) 1)))
	 (def. VECTOR.inc! VECTOR-inc!)

	 (def (VECTOR-dec! v i)
	      (VECTOR-set! v i
			   (- (VECTOR-ref v i) 1)))
	 (def. VECTOR.dec! VECTOR-dec!)

	 ;; could also write (def .sum (C .fold _ + 0)) but then it wouldn't
	 ;; be properly extensible?
	 (def (VECTOR-sum v)
	      (VECTOR.fold v + 0))
	 (def. VECTOR.sum VECTOR-sum)

	 (def (VECTOR-reverse/tail v tail)
	      (error-not-implemented))

	 (def (VECTOR-reverse v)
	      (let* ((len (VECTOR-length v))
		     (out (make-VECTOR len))
		     (len-1 (dec len)))
		(for..< (i 0 len)
			(VECTOR-set! out i
				     (VECTOR-ref v (- len-1 i))))
		out))
	 (def. VECTOR.reverse VECTOR-reverse)))
   
   ;; Could abstract most code into a separate routine that takes a
   ;; make-vector argument, and uses object ops for the rest, but
   ;; those are not optimized at all yet.
   (def (VECTOR-map/iota f v)
	(let* ((len (VECTOR-length v))
	       (out (make-VECTOR len)))
	  (for..< (i 0 len)
		  (VECTOR-set! out i
			       (f (VECTOR-ref v i) i)))
	  out))
   (def. (VECTOR.map/iota v f)
     (VECTOR-map/iota f v))

   (def. (VECTOR.chop-both-ends v)
     (subVECTOR v 1 (dec (VECTOR.length v))))

   (def (VECTOR-for-each proc v)
	(let ((len (VECTOR-length v)))
	  (for..< (i 0 len)
		  (proc (VECTOR-ref v i)))))
   (def. (VECTOR.for-each v proc)
     (VECTOR-for-each proc v))

   ;; directly define dot-oo version as non-dot-oo version will be
   ;; n-ary:
   (def. (VECTOR.map v fn)
     (let* ((len (VECTOR-length v))
	    (res (make-VECTOR len)))
       (for..< (i 0 len)
	       (VECTOR-set! res i (fn (VECTOR-ref v i))))
       res))

   (def. (VECTOR.map-list v fn #!optional (tail '()))
     (let* ((len (VECTOR-length v)))
       (let lp ((l tail)
		(i (dec len)))
	 (if (negative? i)
	     l
	     (lp (cons (fn (VECTOR-ref v i))
		       l)
		 (dec i))))))

   (def (VECTOR-take v k) (subVECTOR v 0 k))
   (def. VECTOR.take VECTOR-take)

   (def (VECTOR-drop v k) (subVECTOR v k (VECTOR-length v)))
   (def. VECTOR.drop VECTOR-drop)


   (def _VECTOR-rest-count 0)

   (def (VECTOR-rest v)
	(let ((len (VECTOR-length v)))
	  (if (zero? len)
	      (error "VECTOR-rest: VECTOR is empty")
	      (begin
		(if (= (inc! _VECTOR-rest-count) 10)
		    (warn "VECTOR-rest is called often, consider optimizing your algorithm")) ;; XX use WARN
		(subVECTOR v 1 len)))))
   (def. VECTOR.rest VECTOR-rest)
   
   (def (VECTOR-first v)
	(let ((len (VECTOR-length v)))
	  (if (zero? len)
	      (error "VECTOR-first: VECTOR is empty")
	      (VECTOR-ref v 0))))
   (def. VECTOR.first VECTOR-first)

   (def (VECTOR-second v)
	(let ((len (VECTOR-length v)))
	  (if (< len 2)
	      (error "VECTOR-second: VECTOR is too small")
	      (VECTOR-ref v 1))))
   (def. VECTOR.second VECTOR-second)

   (def (VECTOR-third v)
	(let ((len (VECTOR-length v)))
	  (if (< len 3)
	      (error "VECTOR-third: VECTOR is too small")
	      (VECTOR-ref v 2))))
   (def. VECTOR.third VECTOR-third)

   (def (VECTOR-last v)
	(let ((len (VECTOR-length v)))
	  (if (zero? len)
	      (error "VECTOR-last: VECTOR is empty")
	      (VECTOR-ref v (dec len)))))
   (def. VECTOR.last VECTOR-last)


   (def (VECTOR-butlast v)
	(subVECTOR v 0 (dec (VECTOR-length v))))
   (def. VECTOR.butlast VECTOR-butlast)

   (def (VECTOR-sublist v si ei)
	(let rec ((i si))
	  (if (< si ei)
	      (cons (VECTOR-ref v i)
		    (rec (inc i)))
	      '())))
   (def. VECTOR.sublist VECTOR-sublist)

   (def (VECTOR-difference s1 s2 #!optional (equal? equal?))
	(error-not-implemented))

   (def (show-VECTOR-difference s1 s2
				#!key
				(equal? equal?)
				(n 2))
	(error-not-implemented))

   (def (VECTOR-append-optimized a b)
	(if (VECTOR.null? a)
	    b
	    (if (VECTOR.null? b)
		a
		(VECTOR-append a b))))
   (def. VECTOR.append-optimized VECTOR-append-optimized)

   (def (VECTOR-append/2 a b)
	;; rely on compiler optim? How does it even work without it?
	(VECTOR-append a b))
   (def. VECTOR.append/2 VECTOR-append/2)

   ;; careful, optimized! (in the sense of append-optimized)
   (def (VECTOR-split-at s [exact-natural0? n]) ;; not supporting  #!optional tail
	(let ((len (VECTOR-length s)))
	  (cond ((zero? n)
		 (values '[] s))
		((= n len)
		 (values s '[]))
		((< n len)
		 (values (subVECTOR s 0 n)
			 (subVECTOR s n len)))
		(else
		 (error "VECTOR-split-at: argument out of bounds:" n)))))
   (def. VECTOR.split-at VECTOR-split-at)

   (def (VECTOR-Maybe-ref v [exact-natural0? i])
	(let ((len (VECTOR-length v)))
	  (if (< i len)
	      (Just (VECTOR-ref v i))
	      (Nothing))))
   (def. VECTOR.Maybe-ref VECTOR-Maybe-ref)

   ;; like stream-min&max in stream.scm
   (def (VECTOR-min&max vec
			#!key
			(cmp generic-cmp)
			all?)
	(let ((con (lambda (v r)
		     (if all? (cons v r) v)))
	      (ex (lambda (vS)
		    (if all? (car vS) vS)))
	      (len (VECTOR-length vec)))

	  (if (zero? len)
	      (error "VECTOR-min&max: got empty VECTOR")

	      (let ((v (VECTOR-ref vec 0)))
		(let lp ((i 1)
			 (min (con v '()))
			 (max (con v '())))
		  (if (< i len)
		      (let ((v (VECTOR-ref vec i)))
			(lp (inc i)
			    (match-cmp (cmp (ex min) v)
				       ((lt) min)
				       ((gt) (con v '()))
				       ((eq) (con v min)))
			    (match-cmp (cmp (ex max) v)
				       ((lt) (con v '()))
				       ((gt) max)
				       ((eq) (con v max)))))
		      (values min max)))))))
   (def. VECTOR.min&max VECTOR-min&max)

   (def VECTOR-min (comp* fst VECTOR-min&max))
   (def VECTOR-max (comp* snd VECTOR-min&max))
   (def. VECTOR.min VECTOR-min)
   (def. VECTOR.max VECTOR-max)

   (def (VECTOR-rtake&rest s n #!optional (tail '()))
	(error-not-implemented))
   ;; (def VECTOR.rtake&rest VECTOR-rtake&rest) why when the generic
   ;; will report that anyway!
   
   ;; n-ary, non-oo version:

   ;; These are already in vector-util!  Not removing them right now
   ;; for fear of dependencies.
   (define (VECTORs-map fn vecs accept-uneven-lengths?)
     (let* ((lens (map VECTOR-length vecs))
	    (len (apply min lens))
	    (cont (lambda ()
		    (let* ((res (##make-VECTOR len)))
		      (let lp ((i 0))
			(if (= i len)
			    res
			    (begin
			      (VECTOR-set! res
					   i
					   (apply fn
						  (map (lambda (vec)
							 (VECTOR-ref vec i))
						       vecs)))
			      (lp (inc i)))))))))
       (if accept-uneven-lengths?
	   (cont)
	   (let ((lenmax (apply max lens)))
	     (if (= len lenmax)
		 (cont)
		 (error "uneven lengths of input VECTORs (min max):"
			len lenmax))))))
   ;; ^XX really getting wasteful with duplication through template-map

   (define (VECTOR-map fn . vecs)
     (VECTORs-map fn vecs #f))

   (define (VECTOR-map* fn . vecs)
     (VECTORs-map fn vecs #t))


   (def (VECTOR-fold-right fn tail vec)
	(let ((len (VECTOR-length vec)))
	  (let rec ((i 0))
	    (if (= i len)
		tail
		(fn (VECTOR-ref vec i)
		    (rec (inc i)))))))
   (def. (VECTOR.fold-right vec fn tail)
     (VECTOR-fold-right fn tail vec))

   (def (VECTOR-fold fn tail vec)
	(let ((len (VECTOR-length vec)))
	  (let lp ((res tail)
		   (i 0))
	    (if (= i len)
		res
		(lp (fn (VECTOR-ref vec i)
			res)
		    (inc i))))))
   (def. (VECTOR.fold vec fn tail)
     (VECTOR-fold fn tail vec))

   ;; Ok this one isn't an OO function; but still fits nicely.
   (def (VECTORs-append strs)
	(let* ((out (##make-VECTOR (sum (map VECTOR-length strs)))))
	  (let lp ((strs strs)
		   (pos 0))
	    (if (null? strs)
		out
		(let-pair ((str strs*) strs)
			  (let ((len (VECTOR-length str)))
			    (for..< (i 0 len)
				    (VECTOR-set! out (+ pos i) (VECTOR-ref str i)))
			    (lp strs* (+ pos len))))))))
   ;; OO-version would be difficult, do what with the empty list? Well can do this:
   (def pair-with-car-VECTOR? (pair-with-car VECTOR?))
   (def. pair-with-car-VECTOR.append VECTORs-append)))



(TEST
 > (.chop-both-ends (u32vector 0 7 0))
 #u32(7)
 > (.chop-both-ends (u32vector 0 7))
 #u32()
 > (.u8vector (map .integer (.list "foo")))
 #u8(102 111 111)
 > (.append '#u8(1 2) '#u8(3 4))
 #u8(1 2 3 4)
 )

(TEST
 > (.fold '#(1 2 3) vector 'null)
 #(3 #(2 #(1 null)))
 > (.fold-right '#(1 2 3) vector 'null)
 #(1 #(2 #(3 null)))
 )


(TEST
 > (strings-append '())
 ""
 > (strings-append '("foo"))
 "foo"
 > (strings-append '("foo" "bar"))
 "foobar"
 > (strings-append '("" "bar"))
 "bar"
 ;;  > (%try-error (strings-append "foo"))
 ;; *** ERROR IN (console)@7.1 -- (Argument 2) LIST expected
 ;; (map '#<procedure #2 string-length> "foo")
 ;; > (strings-append '("" 1))
 ;; *** ERROR IN map -- (Argument 1) STRING expected
 ;; (string-length 1)

 ;; Can't use .append any more since list.append overrides it now
 > (pair-with-car-string.append '("" "bar"))
 "bar"
 ;; and ditto:
 > (pair-with-car-u8vector.append (map .u8vector '("FOO" "BAR")))
 #u8(70 79 79 66 65 82)
 > (.sum (.u8vector "AB"))
 131
 )


;; there's a more efficient string->u8vector in cj-u8vector-util
(def string->u8vector (comp list->u8vector
			    (cut map char->integer <>)
			    string->list))

(def. string.u8vector string->u8vector)

;; also see u8vector->string in cj-u8vector-util
(def. (u8vector.string v)
  (let* ((len (u8vector.length v))
	 (o (##make-string len)))
    (for..< (i 0 len)
	    (string-set!
	     o i
	     (integer->char (u8vector-ref v i))))
    o))

(TEST
 > (.string (u8vector 65 66))
 "AB"
 ;; kinda pointless random tests
 > (def (t v)
	(assert (equal? v (string.u8vector (u8vector.string v)))))
 > (for-each (lambda (l) (t (random-u8vector (* l 13)))) (iota 7)))

(TEST
 > (.reverse (s32vector 43341 -3))
 #s32(-3 43341))


(TEST
 > (.first "Hello")
 #\H
 > (.last "Hello")
 #\o
 > (.rest "Hell")
 "ell"
 ;; Not the best error messages, well.. :
 > (%try (.first ""))
 (exception text: "string-first: string is empty\n")
 > (%try (.last ""))
 (exception text: "string-last: string is empty\n")
 > (%try (.rest ""))
 (exception text: "string-rest: string is empty\n"))

(TEST
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? v)))
 #(2 -4 8)
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? i)))
 #(2 5))

(TEST
 > (def l '())
 > (def v (vector 10 11 12))
 > (.for-each/iota v (lambda (x i)
		       (push! l (cons x i))))
 > l
 ((12 . 2) (11 . 1) (10 . 0)))

(TEST
 > (.filter "Hello, World." char-alpha?)
 "HelloWorld")


(TEST
 > (def a (vector 1 23 3))
 > (eq? a (vector-append a '[]))
 #f ;; in Gambit, at least, but IIRC the standard is even asking for this?
 > (equal? a (vector-append a '[]))
 #t
 > (eq? a (vector-append-optimized a '[]))
 #t
 > (def a "Hi")
 > (eq? a (string-append a ""))
 #f
 > (eq? a (string-append-optimized a ""))
 #t
 > (eq? a (string-append-optimized "" a))
 #t
 > (eq? a (string-append "" a))
 #f ;; ?
 )

(TEST
 > (.show (.min&max '[10 9]))
 (values 9 10)
 > (.show (.min&max '[-10 9]))
 (values -10 9)
 > (.show (.min&max '[-10]))
 (values -10 -10)
 > (%try (.min&max '[]))
 (exception text: "vector-min&max: got empty vector\n")
 > (.show (.min&max '[10 29 4]))
 (values 4 29))

(TEST
 > (.show (.split-at '[a b c] 0))
 (values (vector) (vector 'a 'b 'c))
 > (.show (.split-at '[a b c] 3))
 (values (vector 'a 'b 'c) (vector))
 > (.show (.split-at '[a b c] 2))
 (values (vector 'a 'b) (vector 'c))
 > (%try (.split-at '[a b c] 4))
 (exception text: "vector-split-at: argument out of bounds: 4\n")
 > (%try (.split-at '[a b c] -1))
 (exception text: "n does not match exact-natural0?: -1\n"))

