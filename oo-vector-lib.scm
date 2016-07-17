;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 code-map
	 easy-1)

(export sum

	string->u8vector string.u8vector ;; XX remove this?
	u8vector.string			 ;; dito?

	;; XX why all the "-" versions instead of just the "." versions?

	string.ref
	string.set!
	string.length
	string.list
	list.string
	string.null?
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

	;; the remaining ones have more variants
	vector.ref
	vector.set!
	vector.length
	vector.list
	list.vector
	vector.null?
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

	f32vector.ref
	f32vector.set!
	f32vector.length
	f32vector.list
	list.f32vector
	f32vector.null?
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

	f64vector.ref
	f64vector.set!
	f64vector.length
	f64vector.list
	list.f64vector
	f64vector.null?
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

	u8vector.ref
	u8vector.set!
	u8vector.length
	u8vector.list
	list.u8vector
	u8vector.null?
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

	s8vector.ref
	s8vector.set!
	s8vector.length
	s8vector.list
	list.s8vector
	s8vector.null?
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

	u16vector.ref
	u16vector.set!
	u16vector.length
	u16vector.list
	list.u16vector
	u16vector.null?
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

	s16vector.ref
	s16vector.set!
	s16vector.length
	s16vector.list
	list.s16vector
	s16vector.null?
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

	u32vector.ref
	u32vector.set!
	u32vector.length
	u32vector.list
	list.u32vector
	u32vector.null?
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

	s32vector.ref
	s32vector.set!
	s32vector.length
	s32vector.list
	list.s32vector
	s32vector.null?
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

	u64vector.ref
	u64vector.set!
	u64vector.length
	u64vector.list
	list.u64vector
	u64vector.null?
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

	s64vector.ref
	s64vector.set!
	s64vector.length
	s64vector.list
	list.s64vector
	s64vector.null?
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
	)


(def (sum nums)
     (fold + 0 nums))

(code-map-substrings
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
   (def. list.VECTOR list->VECTOR)

   ;; XX already have |string-empty?|
   (def. (VECTOR.null? v)
     (zero? (VECTOR-length v)))

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
	 (def. (VECTOR.sum v)
	   (VECTOR.fold v + 0))))
   
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
   ;; ^XX really getting wasteful with duplication through code-map-substrings

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
 > (.append '("" "bar"))
 "bar"
 > (.append (map .u8vector '("FOO" "BAR")))
 #u8(70 79 79 66 65 82)
 > (.sum (.u8vector "AB"))
 131
 )


;; there's a more efficient string->u8vector in cj-u8vector-util
(def string->u8vector (comp* list->u8vector
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

