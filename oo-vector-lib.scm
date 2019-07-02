;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require template
	 easy-1
	 test
	 (test-lib-1 %try)
	 char-util
         (fixnum-more fixnum-natural0?))

(export sum
	oo-vector-lib:implement-thunks
	(macro def-oo-vector-lib-for)

	string->u8vector string.u8vector ;; XX remove this?
	u8vector.string			 ;; ditto?
	)

(include "cj-standarddeclares.scm")

(both-times
 ;; (not naming this with stars as it can't be changed at runtime
 ;; without recompiling both oo-vector-lib and its users.)
 (def oo-vector-lib:implement-thunks #f))

(IF oo-vector-lib:implement-thunks
    (begin
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

      (def char-vector? false/1)	   ;; XX evil
      (def source-char-vector? false/1)	   ;; XX evil
      ))

(IF oo-vector-lib:implement-thunks
    (def (error-not-implemented)
	 (error "not yet implemented")))


(def (sum nums)
     (fold + 0 nums))


;; XX why all the "-" versions instead of just the "." versions? Or,
;; more to the point, why not consistently do it for *all* of them?
;; Clean this up.

(deftemplate (def-oo-vector-lib-for VECTOR)

  (def. VECTOR.VECTOR identity)

  (def. VECTOR.ref VECTOR-ref)
  (def. VECTOR.set! VECTOR-set!)

  (def. (VECTOR.swap! v i j)
    (let ((len (VECTOR-length v)))
      (if (and (fixnum? i)
               (fixnum? j)
               (fx< -1 i len)
               (fx< -1 j len))
          (begin
            (unless (##fx= i j)
                    (let ((vi (##VECTOR-ref v i)))
                      (##VECTOR-set! v i (##VECTOR-ref v j))
                      (##VECTOR-set! v j vi)))
            v)
          (error "VECTOR.swap!: i or j not a proper index:" i j))))
  
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
       (declare (block)(standard-bindings)(extended-bindings)(fixnum))
       (let* ((len (VECTOR-length v))
	      (v* (make-VECTOR len)))
	 (let lp ((i 0)
		  (j 0))
	   (if (< i len)
	       (let ((val (##VECTOR-ref v i)))
		 (if (fn val i)
		     (begin
		       (##VECTOR-set! v* j val)
		       (lp (+ i 1) (+ j 1)))
		     (lp (+ i 1) j)))
	       (begin
		 (VECTOR-shrink! v* j)
		 v*)))))
  (def. (VECTOR.filter/iota v fn)
    (VECTOR-filter/iota fn v))

  (def (VECTOR-filter [procedure? fn] [VECTOR? v])
       ;; stupid COPY-PASTE from VECTOR-filter/iota
       (declare (block)(standard-bindings)(extended-bindings)(fixnum)
		(not safe))
       (let* ((len (VECTOR-length v))
	      (v* (make-VECTOR len)))
	 (let lp ((i 0)
		  (j 0))
	   (if (< i len)
	       (let ((val (##VECTOR-ref v i)))
		 (if (fn val)
		     (begin
		       (##VECTOR-set! v* j val)
		       (lp (+ i 1) (+ j 1)))
		     (lp (+ i 1) j)))
	       (let ()
		 (declare (safe))
		 (VECTOR-shrink! v* j)
		 v*)))))
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

	(IF oo-vector-lib:implement-thunks
	    (def (VECTOR-reverse/tail v tail)
		 (error-not-implemented)))

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


  (def *oo-lib-VECTOR:max-warn-inefficient-count* #f)
  (set! *oo-lib-VECTOR:max-warn-inefficient-count* 1000)

  
  (def _VECTOR-drop-count 0)

  (def (VECTOR-drop v k)
       (let ((res (subVECTOR v k (VECTOR-length v))))
	 (begin
	   (when (= (inc! _VECTOR-drop-count)
                    *oo-lib-VECTOR:max-warn-inefficient-count*)
                 ;; XX use WARN or WARN-ONCE
                 (warn "VECTOR-drop is called often, consider optimizing your algorithm")))
	 res))
  (def. VECTOR.drop VECTOR-drop)


  (def _VECTOR-rest-count 0)

  (def (VECTOR-rest v)
       (let ((len (VECTOR-length v)))
	 (if (zero? len)
	     (error "VECTOR-rest: VECTOR is empty")
	     (begin
	       (when (= (inc! _VECTOR-rest-count)
                        *oo-lib-VECTOR:max-warn-inefficient-count*)
                     ;; XX use WARN or WARN-ONCE
                     (warn "VECTOR-rest is called often, consider optimizing your algorithm"))
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

  (def (VECTOR-sublist v [fixnum? si] [fixnum? ei])
       (declare (fixnum))
       (let rec ((i si))
	 (if (< si ei)
	     (cons (VECTOR-ref v i)
		   (rec (+ i 1)))
	     '())))
  (def. VECTOR.sublist VECTOR-sublist)

  (IF oo-vector-lib:implement-thunks
      (def (VECTOR-difference s1 s2 #!optional (equal? equal?))
	   (error-not-implemented)))

  (IF oo-vector-lib:implement-thunks
      (def (show-VECTOR-difference s1 s2
				   #!key
				   (equal? equal?)
				   (n 2))
	   (error-not-implemented)))

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
	       (declare (fixnum))
	       (let lp ((i 1)
			(min (con v '()))
			(max (con v '())))
		 (if (< i len)
		     (let ((v (VECTOR-ref vec i)))
		       (lp (+ i 1)
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

  (IF oo-vector-lib:implement-thunks
      (def (VECTOR-rtake&rest s n #!optional (tail '()))
	   (error-not-implemented)))
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
		     (declare (fixnum))
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
			     (lp (+ i 1)))))))))
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
	 (declare (fixnum))
	 (let rec ((i 0))
	   (if (= i len)
	       tail
	       (fn (VECTOR-ref vec i)
		   (rec (+ i 1)))))))
  (def. (VECTOR.fold-right vec fn tail)
    (VECTOR-fold-right fn tail vec))

  (def (VECTOR-fold fn tail vec)
       (let ((len (VECTOR-length vec)))
	 (declare (fixnum))
	 (let lp ((res tail)
		  (i 0))
	   (if (= i len)
	       res
	       (lp (fn (VECTOR-ref vec i)
		       res)
		   (+ i 1))))))
  (def. (VECTOR.fold vec fn tail)
    (VECTOR-fold fn tail vec))


  ;; also see random:permutate for lists/streams
  (def. (VECTOR.random-permutate! v)
    (let ((len (VECTOR-length v)))
      (for..< (i 0 len)
              (VECTOR.swap! v i (+ i (random-integer (- len i)))))
      v))

  ;; =======================================================
  ;; Non-OO functions 

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
  ;; OO-version would be difficult, do what with the empty list? Well
  ;; can do this:
  (def pair-with-car-VECTOR? (pair-with-car VECTOR?))
  (def. pair-with-car-VECTOR.append VECTORs-append)


  (def ((VECTOR-of-length [fixnum-natural0? len]) v)
       (and (VECTOR? v)
            (##fx= (##VECTOR-length v) len)))

  )

