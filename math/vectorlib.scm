;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cj-functional size0? size?)
	 debuggable-promise)

(include "../cj-standarddeclares.scm")
(possibly-use-debuggable-promise)

;; using prefixes before "." for input types ("dispatch"), before ":"
;; for output types (selection of intent?). ok? (When both are needed,
;; first give the ":" then the "." part.)

;; b = boolean
;; i = integer
;; s = size0 [hm vs size ?]
;; r = real
;; c = complex
;; Vx = real vector of basic type x
;; Mx = 2D matrix of basic type x

;; === Choice of double or float: ===
(define realvector f64vector)
(define realvector-length f64vector-length)
(define realvector-ref f64vector-ref)
(define realvector-ref@ ##f64vector-ref)
(define realvector-set! f64vector-set!)
(define-inline (realvector-set!@ v i x)
  (##f64vector-set! v i x))
(define @make-realvector ##make-f64vector)
(define make-realvector make-f64vector)
(define realvector? f64vector?)
(define list->realvector list->f64vector)
(define realvector->list f64vector->list)
;; === /Choice of double or float ===

(define V_.update!
  (lambda (v i fn)
    (.set! v i (fn (.ref v i)))))
(define V_.update!*
  (lambda (v i fn)
    (V_.update! v (dec i) fn)))

;; === Choice of machine addressing size?: ===
(define Vs u64vector)
(define @make-Vs ##make-u64vector)
(define Vs? u64vector?)
(define. Vs.ref u64vector-ref)
(define. Vs.set! u64vector-set!)
(define. Vs.update! V_.update!)
(define. Vs.update!* V_.update!*)
(define. (Vs.data x) x)
(define. Vs.size u64vector-length)
(define. Vs.->list u64vector->list)
(define list->Vs list->u64vector)
(define. (Vs.show v)
  (cons 'Vs (Vs.->list v)))

(TEST
 > (.show (Vs 0 1 18446744073709551615))
 (Vs 0 1 18446744073709551615)
 )
;; === /Choice of machine addressing size ===

;; === Vector of integer:
;; COPYPASTE from Vs sigh. also, integers with restricted range. bleh.
(define Vi s64vector)
(define @make-Vi ##make-s64vector)
(define Vi? s64vector?)
(define. Vi.ref s64vector-ref)
(define. Vi.set! s64vector-set!)
(define. Vi.update! V_.update!)
(define. Vi.update!* V_.update!*)
(define. (Vi.data x) x)
(define. Vi.size s64vector-length)
(define. Vi.->list s64vector->list)
(define list->Vi list->s64vector)
(define. (Vi.show v)
  (cons 'Vi (Vi.->list v)))

(TEST
 > (.show (Vi 0 -9223372036854775808 9223372036854775807))
 (Vi 0 -9223372036854775808 9223372036854775807)
 )


;; === Vector of real:

(define Vr? realvector?)
(define. Vr.size realvector-length)
(define. Vr.ref realvector-ref)
(define Vr.ref@ realvector-ref@)
(define. Vr.set! realvector-set!)
(define-inline (Vr.set!@ v i x)
  (realvector-set!@ v i x))
(define. Vr.update! V_.update!)
(define. Vr.update!* V_.update!*)
(define @make-Vr @make-realvector)
(define make-Vr make-realvector)
(define list->Vr list->realvector)
(define Vr->list realvector->list)
(define (Vr.data x) x) ;; type check?

(define. (Vr.ref* v i)
  (Vr.ref v (dec i)))

(define. (Vr.set!* v i x)
  (Vr.set! v (dec i) x))

(define (Vr . x)
  (list->Vr (map exact->inexact x)))

(define. (Vr.show v)
  (cons 'Vr (Vr->list v)))

;; === Vector of complex:

(define (fx.twice x)
  (arithmetic-shift x 1))

(define-struct. Vc
  constructor-name: _Vc
  size
  data)

(define. (Vc.ref v i)
  (let ((_i (fx.twice i)))
    (+ (realvector-ref (Vc.data v) _i)
       (* (realvector-ref (Vc.data v) (inc _i)) +1i))))

(define. (Vc.set! v i x)
  (let ((_i (fx.twice i)))
    (realvector-set! (Vc.data v) _i (exact->inexact (real-part x)))
    (realvector-set! (Vc.data v) (inc _i) (exact->inexact (imag-part x)))))

(define. Vc.update! V_.update!)
(define. Vc.update!* V_.update!*)

(define (@make-Vc siz)
  (_Vc siz (@make-realvector (fx.twice siz))))
(define (make-Vc siz)
  (_Vc siz (make-realvector (fx.twice siz))))

(define (list->Vc lis)
  (let* ((siz (length lis))
	 (v (@make-Vc siz))
	 (_v (Vc.data v)))
    (let lp ((i 0)
	     (lis lis))
      (if (null? lis)
	  v
	  (begin
	   (let ((x (car lis)))
	     (realvector-set! _v i (exact->inexact (real-part x)))
	     (realvector-set! _v (inc i) (exact->inexact (imag-part x))))
	   (lp (+ i 2)
	       (cdr lis)))))))

(define (Vc . lis)
  (list->Vc lis))

(define (Vc->list v)
  (let-Vc ((size data) v)
	  (let lp ((i (dec size))
		   (res '()))
	    (if (negative? i)
		res
		(lp (dec i)
		    (cons (+ (realvector-ref data (fx.twice i))
			     (* (realvector-ref data (inc (fx.twice i))) +i))
			  res))))))

(define. (Vc.show v)
  (cons 'Vc (Vc->list v)))

(TEST
 > (Vr 2 3 -4.5)
 #f64(2. 3. -4.5)
 > (Vr.show #)
 (Vr 2. 3. -4.5)
 > (Vc 1 2-3i)
 #((Vc) 2 #f64(1. 0. 2. -3.))
 > (Vc.show #)
 (Vc 1.+0.i 2.-3.i)
 )

(TEST
 > (define v (Vc 1 2 3))
 > (.update!* v 3 inc*)
 > (.show v)
 (Vc 1.+0.i 2.+0.i 4.+0.i)
 > (define m (Mr (Vr 1 2 3) (Vr 4 5 6)))
 > (.update!* m 1 2 -)
 > (.show m)
 (Mr (Vr 1. -2. 3.) (Vr 4. 5. 6.))
 )

;; ==== common library

;; XX slow. but, shouldn't it be kept abstract anyway?
(define (V_:iota-inexact list->V_)
  (lambda args
    (list->V_ (map exact->inexact (apply iota args)))))
(define (V_:iota-exact list->V_)
  (lambda args
    (list->V_ (apply iota args))))


(define Vr:iota (V_:iota-inexact list->Vr))
(define Vc:iota (V_:iota-inexact list->Vc))
(define Vi:iota (V_:iota-exact list->Vi))
(define Vs:iota (V_:iota-exact list->Vs))

(TEST
 > (Vr:iota 2)
 #f64(0. 1.)
 > (Vc:iota 2)
 #((Vc) 2 #f64(0. 0. 1. 0.)) ;; hm yeah kinda pointless?
 )

(define (.. from to)
  (iota (inc (- to from)) from))

(define (V_:.. V_:iota)
  (lambda (from to) ;; inclusive
    (V_:iota (inc (- to from)) from)))

(define Vr:.. (V_:.. Vr:iota))
(define Vc:.. (V_:.. Vc:iota))
(define Vi:.. (V_:.. Vi:iota))
(define Vs:.. (V_:.. Vs:iota))

(TEST
 > (Vr:.. 2 4)
 #f64(2. 3. 4.)
 > (Vc:.. 2 4)
 #((Vc) 3 #f64(2. 0. 3. 0. 4. 0.))
 )


;; === map of a vector

(define (V_.map V_.size make-V_@ V_.ref V_.set!)
  (lambda (v fn)
    (let* ((len (V_.size v))
	   (res (make-V_@ len)))
      (let lp ((i 0))
	(when (< i len)
              (V_.set! res i (fn (V_.ref v i)))
	      (lp (inc i))))
      res)))

(define. Vr.map (V_.map Vr.size @make-Vr Vr.ref Vr.set!))
(define. Vc.map (V_.map Vc.size @make-Vc Vc.ref Vc.set!))
(define. Vi.map (V_.map Vi.size @make-Vi Vi.ref Vi.set!))
(define. Vs.map (V_.map Vs.size @make-Vs Vs.ref Vs.set!))
;; === map of a vector to another type
(define. Vr:Vc.map (V_.map Vc.size @make-Vr Vc.ref Vr.set!))
(define. Vc:Vr.map (V_.map Vr.size @make-Vc Vr.ref Vc.set!))
(define. Vc:Vc.map (V_.map Vc.size @make-Vc Vc.ref Vc.set!))

(define (Vrr.* v x) (Vr.map v (cut * <> x)))
(define (Vrr./ v x) (Vr.map v (cut / <> x)))
(define (Vrr.+ v x) (Vr.map v (cut + <> x)))
(define (Vrr.- v x) (Vr.map v (cut - <> x)))

(define (Vcr.* v x) (Vc.map v (cut * <> x)))
(define (Vcr./ v x) (Vc.map v (cut / <> x)))
(define (Vcr.+ v x) (Vc.map v (cut + <> x)))
(define (Vcr.- v x) (Vc.map v (cut - <> x)))

(define (Vii.* v x) (Vi.map v (cut * <> x)))
(define (Vii.quotient v x) (Vi.map v (cut quotient <> x)))
(define (Vii.+ v x) (Vi.map v (cut + <> x)))
(define (Vii.- v x) (Vi.map v (cut - <> x)))

(define (Vss.* v x) (Vs.map v (cut * <> x)))
;;(define (Vsi./ v x) (Vs.map v (cut / <> x)))
(define (Vsi.+ v x) (Vs.map v (cut + <> x)))
(define (Vsi.- v x) (Vs.map v (cut - <> x)))

;; XX stupid? and should they error-check?; or even: should r be a
;; subset of c. Well, the representation thing is the issue (we're not
;; bothering whether imag==0 but we do about repr. in C).
(define Vcc.* Vcr.*)
(define Vcc./ Vcr.*)
(define Vcc.+ Vcr.*)
(define Vcc.- Vcr.*)

(define (Vc:Vrc.* v x) (Vc:Vr.map v (cut * <> x)))
(define (Vc:Vrc./ v x) (Vc:Vr.map v (cut / <> x)))
(define (Vc:Vrc.+ v x) (Vc:Vr.map v (cut + <> x)))
(define (Vc:Vrc.- v x) (Vc:Vr.map v (cut - <> x)))

(TEST
 > (Vrr.* (Vr 2 4) 3)
 #f64(6. 12.)
 > (Vcr.* (Vc 2 4-2i) 3)
 #((Vc) 2 #f64(6. 0. 12. -6.))
 > (Vcr.+ (Vc 2 4-2i) 3)
 #((Vc) 2 #f64(5. 0. 7. -2.))
 > (Vc:Vrc.- (Vr 3 4) 1.-2.i)
 #((Vc) 2 #f64(2. 2. 3. 2.))
 )

;;(XX have vector map now, reimplement in terms of those?)
(define (V_V_._ @make-V_ op)
  ;; XX the slow way
  (lambda (a b)
    ;; XX should check type of b? anyway.
    (assert (= (.size a) (.size b)))
    (let* ((size (.size a))
	   (res (@make-V_ size)))
      (for..< (i 0 size)
	      (.set! res i (op (.ref a i) (.ref b i))))
      res)))

(define VrVr.+ (V_V_._ @make-Vr +))
(define VrVr.- (V_V_._ @make-Vr -))
(define VrVr.* (V_V_._ @make-Vr *))
(define VrVr./ (V_V_._ @make-Vr /))

(define VcVc.+ (V_V_._ @make-Vc +))
(define VcVc.- (V_V_._ @make-Vc -))
(define VcVc.* (V_V_._ @make-Vc *))
(define VcVc./ (V_V_._ @make-Vc /))

(define ViVi.+ (V_V_._ @make-Vi +))
(define ViVi.- (V_V_._ @make-Vi -))
(define ViVi.* (V_V_._ @make-Vi *))
(define ViVi./ (V_V_._ @make-Vi /))

(define VsVs.+ (V_V_._ @make-Vs +))
(define VsVs.- (V_V_._ @make-Vs -))
(define VsVs.* (V_V_._ @make-Vs *))
(define VsVs./ (V_V_._ @make-Vs /))

(define Vs:VsVi.+ (V_V_._ @make-Vs +))
(define Vs:VsVi.- (V_V_._ @make-Vs -))
(define Vs:VsVi.* (V_V_._ @make-Vs *))
(define Vs:VsVi./ (V_V_._ @make-Vs /))
;; explosion really; generate on demand?.... (~same as generate all
;; then eliminiate unused ones h; or well use casting in C, at least
;; for unsafe code)


(TEST
 > (VrVr.+ (Vr 1 2) (Vr 3 4))
 #f64(4. 6.)
 > (VrVr./ (Vr 1 2) (Vr 3 4))
 #f64(.3333333333333333 .5)
 )

(define (V_.for-each v proc)
  (let* ((len (.size v)))
    (for..< (i 0 len)
	    (proc (Vr.ref v i)))))

(define. Vr.for-each V_.for-each)
(define. Vc.for-each V_.for-each)


(define (V_.fold _.size _.ref)
  (lambda (v fn start)
    (let* ((len (_.size v)))
      (let lp ((i 0)
	       (res start))
	(if (< i len)
	    (lp (inc i)
		(fn (_.ref v i) res))
	    res)))))

(define. Vr.fold (V_.fold Vr.size Vr.ref))
(define. Vc.fold (V_.fold Vc.size Vc.ref))


(define (_:generate @make)
  (lambda (r:fn siz)
    (let ((v (@make siz)))
      (for..< (i 0 siz)
	      (.set! v i (r:fn i)))
      v)))

(define Vr:generate (_:generate @make-Vr))
(define Vi:generate (_:generate @make-Vi))
(define Vs:generate (_:generate @make-Vs))
(define Vc:generate (_:generate @make-Vc))

(define (zero/1 _)
  0)
(define (one/1 _)
  1)
(define (r:zero/1 _)
  0.)
(define (r:one/1 _)
  1.)

(define Vr:zeros
  (cut Vr:generate r:zero/1 <>))

(define Vr:ones
  (cut Vr:generate r:one/1 <>))

(define Vc:zeros
  ;; relying on propagation of r to c
  (cut Vc:generate r:zero/1 <>))

(define Vc:ones
  ;; relying on propagation of r to c
  (cut Vc:generate r:one/1 <>))

(define Vi:zeros
  (cut Vi:generate zero/1 <>))
(define Vi:ones
  (cut Vi:generate one/1 <>))

(define Vs:zeros
  (cut Vs:generate zero/1 <>))
(define Vs:ones
  (cut Vs:generate one/1 <>))


;; a matrix is a vector of a Vx for now (with wrapper struct)

(define-macro* (define-M_ t)
  ;; (define-macro-symbol-replace-_-with R t) doesn't work with quasiquote-source
  (define T (symbol-replace-_-with/ t))
  ;; sigh underscore in _Mx conflicts with that replacement idea..:
  (let ((_M_ (symbol-append '_M (string* t))))
    (quasiquote-source
     (begin
       (define-struct. ,(T 'M_)
	 constructor-name: ,(symbol-append '_M (string* t))
	 size0
	 size1
	 data)

       (define-typed (,(T 'M_) . #((both pair? (list-of ,(T 'V_?))) vs))
	 (let ((s0 (length vs))
	       (s1 (,(T 'V_.size) (car vs))))
	   (for-each (lambda (v)
		       (assert (= (,(T 'V_.size) v) s1)))
		     vs)
	   (,_M_ s0 s1 (list->vector vs))))

       (define-typed (,(T '@make-M_) #(size? s0) #(size? s1))
	 ;; with separate (uninitialized) rows so as to make them
	 ;; overwritable
	 (,_M_ s0 s1 (vector-generate s0
				      (lambda (_)
					(,(T '@make-V_) s1)))))

       (define. (,(T 'M_.ref) m i0 i1)
	 (,(T 'V_.ref) (vector-ref (,(T 'M_.data) m) i0) i1))

       (define-inline (,(T 'M_.ref@) m i0 i1)
	 (declare (not safe))
	 (,(T 'V_.ref@) (##vector-ref (,(T '@M_.data) m) i0) i1))
       
       (define. (,(T 'M_.ref*) m i0 i1)
	 (,(T 'V_.ref) (vector-ref (,(T 'M_.data) m) (dec i0)) (dec i1)))

       (define. (,(T 'M_.sizes) m)
	 (values (,(T 'M_.size0) m)
		 (,(T 'M_.size1) m)))

       (define. ,(T 'M_.size)
	 (typed-lambda
	  (m #((both natural0? (cut < <> 2)) dim))
	  ;; assumes all rows are the same length!
	  (case dim
	    ((0) (,(T 'M_.size0) m))
	    ((1) (,(T 'M_.size1) m)))))

       (define (,(T 'list->M_) listoflists)
	 ;; exact->inexact also accepts inexact numbers. Lucky.
	 (let ((s0 (length listoflists))
	       (s1 (length (car listoflists))))
	   (,_M_
	    s0 s1
	    (list->vector
	     (map (compose-function ,(T 'list->V_) (cut map exact->inexact <>))
		  listoflists)))))

       ;; should this be done w a R.unfold?
       (define (,(T 'M_:generate/rows) V_:fn siz)
	 ;; almost COPY of V:generate
	 (let ((v (make-vector siz)))
	   (let lp ((i 0))
	     (when (< i siz)
                   (vector-set! v i (V_:fn i))
		   (lp (inc i))))
	   (,_M_
	    siz
	    (,(T 'V_.size) (vector-ref v 0)) ;; assume all are the same.
	    v)))
       ;; offer a R:generate/rows* variant that passes indizes starting from 1?

       (define (,(T 'M_:zeros) s0 s1)
	 ;; (make-vector y
	 ;; 	       (V.zeros x))
	 ;;XXXwrong, mutation.. --- hm? or how to handle this case?
	 (,_M_ s0 s1 (list->vector
		      (map (lambda (_)
			     (,(T 'V_:zeros) s1))
			   (iota s0)))))

       (define (,(T 'M_:generate) fn/2 s0 s1)
	 (,_M_ s0 s1 (list->vector
		      (map (lambda (i0)
			     (,(T 'V_:generate)
			      (lambda (i1)
				(fn/2 i0 i1))
			      s1))
			   (iota s0)))))

       (define (,(T 'M_:ones) s0 s1)
	 ;;~ditto
	 (,_M_ s0 s1 (list->vector
		      (map (lambda (_)
			     (,(T 'V_:ones) s1))
			   (iota s0)))))
       (define. (,(T 'M_.set!) m i0 i1 v)
	 (,(T 'V_.set!) (vector-ref (,(T 'M_.data) m) i0) i1 v))
       (define-inline (,(T 'M_.set!@) m i0 i1 v)
	 (declare (not safe))
	 (,(T 'V_.set!@) (##vector-ref (,(T '@M_.data) m) i0) i1 v))
       (define. (,(T 'M_.set!*) m i0 i1 v)
	 (,(T 'V_.set!) (vector-ref (,(T 'M_.data) m) (dec i0)) (dec i1) v))
       (define. (,(T 'M_.update!) m i0 i1 fn)
	 (,(T 'V_.update!) (vector-ref (,(T 'M_.data) m) i0) i1 fn))
       (define. (,(T 'M_.update!*) m i0 i1 fn)
	 (,(T 'V_.update!*) (vector-ref (,(T 'M_.data) m) (dec i0)) i1 fn))

       (define. (,(T 'M_.show) m)
	 (,(T 'let-M_) ((size0 size1 data) m)
	  ;; since show is going to be used for testing, fsck'ing
	  ;; makes sense here, ok? (or just warn?)
	  (assert (= (vector-length data) size0))
	  (cons ',(T 'M_)
		(map (lambda (v)
		       (assert (= (,(T 'V_.size) v) size1))
		       (,(T 'V_.show) v))
		     (vector->list data)))))))))

(define-M_ #\r)
(define-M_ #\c)


(TEST
 > (Mr:zeros 2 3)
 #((Mr) 2 3 #(#f64(0. 0. 0.) #f64(0. 0. 0.)))
 > (.show (Mr:zeros 2 3))
 (Mr (Vr 0. 0. 0.) (Vr 0. 0. 0.))
 > (Mr:ones 1 3)
 #((Mr) 1 3 #(#f64(1. 1. 1.)))
 > (.show (Mr (Vr 1 2) (Vr 3 4)))
 (Mr (Vr 1. 2.) (Vr 3. 4.))
 > (.show (Mc (Vc 1 2) (Vc 3-2i 4)))
 (Mc (Vc 1.+0.i 2.+0.i) (Vc 3.-2.i 4.+0.i))
 )

(define (M_.fold V_.fold M_.data)
  (lambda (m fn start)
    (vector-fold
     (lambda (v res)
       (V_.fold v fn res))
     start
     (M_.data m))))

(define. Mr.fold (M_.fold Vr.fold Mr.data))
(define. Mc.fold (M_.fold Vc.fold Mc.data))

;; (define. (Mr.min+max m)
;;   (let ((first (Mr.ref m 0 0)))
;;     (Mr.fold m
;; 	     (lambda (v lo.hi)
;; 	       (with-values lo.hi
;; 		 (values (min lo v)
;; 			 (max hi v))))
;; 	     (values first first))))
;;XX speed hack:
(define. (Mr.min+max m)
  (letv ((s0 s1) (Mr.sizes m))
	(let* ((first (Mr.ref m 0 0))
	       (lo first)
	       (hi first))
	  (declare (fixnum) (not safe))
	  (for..< (i0 0 s0)
		  (for..< (i1 0 s1)
			  (let ((v (Mr.ref@ m i0 i1)))
			    (when (fl< v lo)
                                  (set! lo v))
			    (when (fl> v hi)
                                  (set! hi v)))))
	  (values lo hi))))


(define. (Vr.min+max v)
  (let ((first (.ref v 0)))
    (.fold v
	   (lambda (x lo.hi)
	     (with-values lo.hi
	       (values (min lo x)
		       (max hi x))))
	   (values first first))))

(define-typed (Mr.print #(Mr? m))
  (print "[")
  (vector-for-each
   (lambda (v)
     (print "[")
     (let ((space ""))
       (Vr.for-each v
		    (lambda (x)
		      (display space)
		      (display x)
		      (set! space " "))))
     (println "]"))
   m)
  (println "]"))

;; ===


;; 	sig2 = zeros(Z,samplingX*(X-1)+1); % a zero array is generated having
;; 					   % the appropriate size
;; 	sig2(:,[0:X-1]*samplingX+1) = sig(:,:); % signal lines corresponding to
;; 						% real elements are filled
(define-typed (Mri.spread-out #(Mr? m) #(natural? spread))
  ;; only adds zero rows *between* original rows, not at the end.
  ;;XX sharing row data, careful..
  (let* ((zerorow (make-Vr (Mr.size m 1)))
	 (res (vector-fold-right
	       (lambda (x res)
		 (let lp ((res (cons x res))
			  (n spread))
		   (if (= n 1)
		       res
		       (lp (cons zerorow res)
			   (dec n)))))
	       '()
	       (Mr.data m))))
    ;; wastefully drop rows at edge again
    (_Mr
     (dec (* spread (Mr.size0 m)))
     (Mr.size1 m)
     (list->vector ;;XX really not fast...
      (drop res (dec spread))))))

(TEST
 > (Mri.spread-out (list->Mr '((1 2.4) (3 4) (5 6))) 2)
 #((Mr) 5 2 ;; sizey is really redundant. hm
   #(#f64(1. 2.4)
	 #f64(0. 0.)
	 #f64(3. 4.)
	 #f64(0. 0.)
	 #f64(5. 6.)))
 )

;; -- Function File: [Y1, Y2, ..., Yn] = ndgrid (X1, X2, ..., Xn)
;; -- Function File: [Y1, Y2, ..., Yn] = ndgrid (X)
;;     Given n vectors X1, ... Xn, `ndgrid' returns n arrays of dimension
;;     n. The elements of the i-th output argument contains the elements
;;     of the vector Xi repeated over all dimensions different from the
;;     i-th dimension.  Calling ndgrid with only one input argument X is
;;     equivalent of calling ndgrid with all n input arguments equal to X:
;;
;;     [Y1, Y2, ...,  Yn] = ndgrid (X, ..., X)
;;
;;     See also: meshgrid

;; Hm. It seems to ignore X3.. totally. From X2, it just takes the
;; length and uses it as the replication factor (width of the result);
;; if not given, makes the result a square. X1 is used as a column
;; (not row; i.e. vertical).
;; Aha that's when expecting just 1 result value. Hm hu.

;; hm. For now, just implement this:
;; >>> [kz,kx]= ndgrid([1,2,3],[-4,-5,-6])
;; kz =
;;    1   1   1
;;    2   2   2
;;    3   3   3
;; kx =
;;   -4  -5  -6
;;   -4  -5  -6
;;   -4  -5  -6
;; >>> [x,y] = ndgrid([1,2,3],[4,5])
;; x =
;;    1   1
;;    2   2
;;    3   3
;; y =
;;    4   5
;;    4   5
;;    4   5

(define (VrVr.ndgrid x1 x2)
  (let ((s1 (Vr.size x1))
	(s2 (Vr.size x2)))
    (values (.replicate-column x1 s2)
	    (.replicate-row x2 s1))))

(define. (Vr.replicate-row v n)
  (_Mr n
       (Vr.size v)
       (make-vector n v)))

(define. (vector:Vr.map v fn)
  (let* ((s (Vr.size v))
	 (res (make-vector s)))
    (let lp ((i 0))
      (when (< i s)
            (vector-set! res i (fn (Vr.ref v i)))
	    (lp (inc i))))
    res))

(define. (Vr.replicate-column v n)
  (_Mr (Vr.size v) n
       (vector:Vr.map v
		      (cut make-Vr n <>))))

(TEST
 > (.show (.replicate-row (Vr 1 2 3) 3))
 (Mr (Vr 1. 2. 3.)
     (Vr 1. 2. 3.)
     (Vr 1. 2. 3.))
 > (.show (.replicate-column (Vr 1 2 3) 3))
 (Mr (Vr 1. 1. 1.) (Vr 2. 2. 2.) (Vr 3. 3. 3.))
 > (map .show (values->list (VrVr.ndgrid (Vr 1 2 3) (Vr -4 -5 -6))))
 ((Mr (Vr 1. 1. 1.)
      (Vr 2. 2. 2.)
      (Vr 3. 3. 3.))
  (Mr (Vr -4. -5. -6.)
      (Vr -4. -5. -6.)
      (Vr -4. -5. -6.)))
 )

;; === map of two vectors

(define (VrVr.map v1 v2 fn)
  (let* ((len (Vr.size v1))
	 (res (@make-Vr len)))
    (assert (= len (Vr.size v2)))
    (let lp ((i 0))
      (when (< i len)
            (Vr.set! res i (fn (Vr.ref v1 i)
			       (Vr.ref v2 i)))
	    (lp (inc i))))
    res))

;; === map of two matrices

(define (_:M_M_.map+/ @make-M_)
  (lambda (m1 m2 fn)
    (let-values (((x y) (.sizes m1))
		 ((x2 y2) (.sizes m2)))
      (assert (= x x2))
      (assert (= y y2))
      (let ((res (@make-M_ x y)))
	(for..< (i0 0 x)
		(for..< (i1 0 y)
			(.set! res i0 i1
			       (fn (.ref m1 i0 i1)
				   (.ref m2 i0 i1)
				   i0
				   i1))))
	res))))

(define (_:M_M_.map/ @make-M_)
  (let ((_map (_:M_M_.map+/ @make-M_)))
    (lambda (m1 m2 fn)
      (_map m1 m2 (lambda (x1 x2 i0 i1)
		    (fn x1 x2))))))

(define MrMr.map (_:M_M_.map/ @make-Mr))
(define McMc.map (_:M_M_.map/ @make-Mc))
;; or generally:
(define Mr:.map-2 (_:M_M_.map/ @make-Mr))
(define Mc:.map-2 (_:M_M_.map/ @make-Mc))

(define MrMr.map+ (_:M_M_.map+/ @make-Mr))
(define McMc.map+ (_:M_M_.map+/ @make-Mc))
;; or generally:
(define Mr:.map+-2 (_:M_M_.map+/ @make-Mr))
(define Mc:.map+-2 (_:M_M_.map+/ @make-Mc))


;; === map of one matrix

(define (_:.map+/ @make-M_)
  (lambda (m fn)
    (letv ((x y) (.sizes m))
	  (let ((res (@make-M_ x y)))
	    (for..< (i0 0 x)
		    (for..< (i1 0 y)
			    (.set! res i0 i1
				   (fn (.ref m i0 i1) i0 i1))))
	    res))))
(define (_:.map/ @make-M_)
  (let ((_map (_:.map+/ @make-M_)))
    (lambda (m fn)
      (_map m (lambda (x i0 i1)
		(fn x))))))

;; == map of one matrix, no indices

(define Mr:.map (_:.map/ @make-Mr))
(define Mc:.map (_:.map/ @make-Mc))
;;XX don't type check restrictively :
(define. Mr.map Mr:.map)
(define. Mc.map Mc:.map)
(define Mc:Mr.map Mc:.map)
(define Mr:Mc.map Mr:.map)

;; == map of one matrix, with indices

(define Mr:.map+ (_:.map+/ @make-Mr))
(define Mc:.map+ (_:.map+/ @make-Mc))
;;XX don't type check restrictively :
(define. Mr.map+ Mr:.map+)
(define. Mc.map+ Mc:.map+)
(define Mc:Mr.map+ Mc:.map+)
(define Mr:Mc.map+ Mr:.map+)


(TEST
 > (.show (.map (Vr 1 2 3) inc*))
 (Vr 2. 3. 4.)
 > (.show (.map (Vc 1 2 3) inc*))
 (Vc 2.+0.i 3.+0.i 4.+0.i)
 > (.show (.map (Mr (Vr 1 2) (Vr 3 4)) inc*))
 (Mr (Vr 2. 3.) (Vr 4. 5.))
 > (.show (.map+ (Mr (Vr 1 2) (Vr 3 4)) (lambda (x i0 i1) (+ x i0))))
 (Mr (Vr 1. 2.) (Vr 4. 5.))
 )

;; previously used definition
;; (define. (Mr.map m fn)
;;   (letv ((x y) (Mr.sizes m))
;; 	(_Mr x y (vector-map (cut Vr.map <> fn)
;; 			     (Mr.data m)))))

(define Mr..square
  (cut Mr.map <> square))

(TEST
 > (define tesv (Mr (Vr 1 2 3) (Vr 4 5 -6)))
 > tesv
 #((Mr) 2 3 #(#f64(1. 2. 3.) #f64(4. 5. -6.)))
 > (Mr..square #)
 #((Mr) 2 3 #(#f64(1. 4. 9.) #f64(16. 25. 36.)))
 )

(define Mr..sqrt
  (cut Mr.map <> sqrt))

(define Mr.neg
  (cut Mr.map <> -))


;; XX these don't check their arguments restrictively anymore!
(define MrMr..+ (cut MrMr.map <> <> +))
(define MrMr..- (cut MrMr.map <> <> -))
(define MrMr..* (cut MrMr.map <> <> *))
(define MrMr../ (cut MrMr.map <> <> /))
;; thus:
(define Mr:..+ (cut MrMr.map <> <> +))
(define Mr:..- (cut MrMr.map <> <> -))
(define Mr:..* (cut MrMr.map <> <> *))
(define Mr:../ (cut MrMr.map <> <> /))

(define McMc..+ (cut McMc.map <> <> +))
(define McMc..- (cut McMc.map <> <> -))
(define McMc..* (cut McMc.map <> <> *))
(define McMc../ (cut McMc.map <> <> /))

;; or generally:
(define Mc:..+ (cut Mc:.map-2 <> <> +))
(define Mc:..- (cut Mc:.map-2 <> <> -))
(define Mc:..* (cut Mc:.map-2 <> <> *))
(define Mc:../ (cut Mc:.map-2 <> <> /))


(TEST
 > (MrMr..+ tesv tesv)
 #((Mr) 2 3 #(#f64(2. 4. 6.) #f64(8. 10. -12.)))
 > (MrMr../ tesv tesv)
 #((Mr) 2 3 #(#f64(1. 1. 1.) #f64(1. 1. 1.)))
 > (Mr.set!* tesv 2 3 -1.)
 > tesv
 #((Mr) 2 3 #(#f64(1. 2. 3.) #f64(4. 5. -1.)))
 )

(define (rr.mod a b)
  (- a (* (floor (/ a b)) b)))

(TEST
 > (rr.mod 10.1 3.)
 1.0999999999999996 ;;well
 )

(define. (Mr..mod m x)
  (Mr.map m (cut rr.mod <> x)))

(define. (Mr..+ m x)
  (Mr.map m (cut + <> x)))
(define. (Mr..- m x)
  (Mr.map m (cut + <> x)))
(define. (Mr..* m x)
  (Mr.map m (cut * <> x)))
(define. (Mr../ m x)
  (Mr.map m (cut / <> x)))


(define (mk-transpose @make-M_)
  (lambda (m)
    ;; XX not very efficient.
    (letv ((s0 s1) (.sizes m))
	  (let ((res (@make-M_ s1 s0)))
	    (for..< (i0 0 s0)
		    (for..< (i1 0 s1)
			    (.set! res i1 i0
				   (.ref m i0 i1))))
	    res))))

(define. Mr.transpose (mk-transpose @make-Mr))
(define. Mc.transpose (mk-transpose @make-Mc))

(TEST
 > (.show (.transpose (Mr (Vr 1 2 3) (Vr 4 5 -6))))
 (Mr (Vr 1. 4.) (Vr 2. 5.) (Vr 3. -6.))
 > (.show (.transpose (Mc (Vc 1 2 3) (Vc 4 5 0-6i))))
 (Mc (Vc 1.+0.i 4.+0.i) (Vc 2.+0.i 5.+0.i) (Vc 3.+0.i 0.-6.i))
 )


(define (_.sum _.fold)
  (lambda (v)
    (_.fold v + 0)))

(define. Vr.sum (_.sum Vr.fold))
(define. Vc.sum (_.sum Vc.fold))

;; wanted to call it Vc:Mc.ref*, but realizing, Vc:McVii.ref* to
;; differ from the other dimension being a vector, thus, and since
;; result is Vc anyway, and since dynamically selecting would be safe
;; too also (hm, only if it does multi dispatch tho!!!)..

;; ("hm should I call it slice-ref* ? ref-slice* ? but which
;; dimension, won't work dyndispatched w/o multi dispatch anyway.")

;; ah name it in consistency with .set-at!

(define (M_Vss.ref-at* m v0 i1)
  (let* ((siz (Vs.size v0))
	 (res (@make-Vc siz)))
    (for..< (i 0 siz)
	    (.set! res i (.ref* m (.ref v0 i) i1)))
    res))

(define MrVss.ref-at* M_Vss.ref-at*)
(define McVss.ref-at* M_Vss.ref-at*)

;; octave:12> m=[1,2,3;4,5,6;7,8,9]
;; octave:13> v=[2,3,1]
;; octave:14> m(v,3)
;; ans =
;;    6
;;    9
;;    3

(TEST
 > (define m (Mc (Vc 1 2 3) (Vc 4 5 6) (Vc 7 8 9)))
 > (.show (McVss.ref-at* m (Vs 2 3 1) 3))
 (Vc 6.+0.i 9.+0.i 3.+0.i)
 )


;; ====  vectors of booleans ===================================

(define-typed (b.identity #(boolean? v))
  v)

(define-struct. Vb
  constructor-name: _Vb
  size
  data)
;; well sfz everything again? macros?


(define (Vb.ref v i)
  (b.identity (vector-ref (Vb.data v) i)))

(define-typed (Vb.set! v i #(boolean? x))
  (vector-set! (Vb.data v) i x))

(define (Vb . vals)
  (let ((v (apply vector (map b.identity vals))))
    (_Vb (vector-length v) v)))

(define (make-Vb@ len)
  (_Vb len (make-vector len)))

(define (Vb:.map v fn) ;; MOSTLY COPY
  (let* ((len (.size v))
	 (res (make-Vb@ len)))
    (let lp ((i 0))
      (when (< i len)
            (Vb.set! res i (fn (.ref v i)))
	    (lp (inc i))))
    res))

(define. Vb:Vr.map Vb:.map)
(define. Vb:Vc.map Vb:.map)

(define (.set-at! res is xs)
  (assert ((on vectorlib:type-of equal?) res xs))
  (let ((ni (Vb.size is)))
    (assert (= ni (.size xs)))
    (assert (<= ni (.size res)))
    (for..< (i 0 ni)
	    (when (Vb.ref is i)
                  (.set! res i (.ref xs i))))))

;;(define. VrVbVr.set-at! .set-at!) well
;;(define. VcVbVc.set-at! .set-at!) well

(TEST
 > (define v (Vr 1 2 3 5 10))
 > (define b (Vb:Vr.map v even?))
 > (.set-at! v b (Vr.map (Vr:iota 5) -))
 > v
 #f64(1. -1. 3. 5. -4.)
 )

(define (M_.for-each* m proc/3)
  (letv ((s0 s1) (.sizes m))
	(for..< (i0 0 s0)
		(for..< (i1 0 s1)
			(proc/3 i0 i1 (.ref m i0 i1))))))
;;XX why did I choose name with * ? i0 and i1 are zero based, no?

(define. Mr.for-each* M_.for-each*)
(define. Mc.for-each* M_.for-each*)



(define (M_.section @make-M_)
  (typed-lambda
   (m #(size0? from0) to0
      #(size0? from1) to1)
   (letv ((s0 s1) (.sizes m))
	 (assert (<= to0 s0))
	 (assert (<= to1 s1))
	 (let* ((s0* (- to0 from0))
		(s1* (- to1 from1))
		(res (@make-M_ s0* s1*)))
	   (for..< (i0 from0 to0)
		   (for..< (i1 from1 to1)
			   (.set! res
				  (- i0 from0)
				  (- i1 from1)
				  (.ref m i0 i1))))
	   res))))

(define (M_.section* @make-M_)
  (let ((.section (M_.section @make-M_)))
    (typed-lambda (m #(size? from0) to0
		     #(size? from1) to1)
		  (.section m
			    (dec from0)
			    to0
			    (dec from1)
			    to1))))

;; XX again no type enforcement
(define. Mr.section (M_.section @make-Mr))
(define. Mc.section (M_.section @make-Mc))
(define. Mr.section* (M_.section* @make-Mr))
(define. Mc.section* (M_.section* @make-Mc))

(TEST
 > (.show (.section* (Mr (Vr 1 2 3)
			 (Vr 4 5 6)) 1 2 2 2))
 ;; m([1:2],[2:2])
 (Mr (Vr 2.)
     (Vr 5.))
 > (.show (.section* (Mr (Vr 1 2 3)
			 (Vr 4 5 6)
			 (Vr 7 8 9)) 2 3 2 3))
 ;;  m([2:3],[2:3])
 (Mr (Vr 5. 6.)
     (Vr 8. 9.)))


(define (fftshift-shift i s n)
  (let ((i* (+ i n)))
    (if (>= i* s)
	(- i* s)
	i*)))

(define (Mc._fftshift/ shift0 shift1 ishift0 ishift1)
  (lambda (m)
    (letv ((s0 s1) (Mc.sizes m))
	  (let ((res (@make-Mc s0 s1))
		(n0 (quotient s0 2))
		(n1 (quotient s1 2)))
	    (for..< (i0 0 s0)
		    (for..< (i1 0 s1)
			    (.set! res
				   (shift0 i0 s0 n0)
				   (shift1 i1 s1 n1)
				   (.ref m
					 (ishift0 i0 s0 n0)
					 (ishift1 i1 s1 n1)))))
	    res))))

;; only shift s0, not s1, since s1 is reduced anyway after fft.
(define fftshift-identity (lambda (i s n) i))

(define. Mc.fftshift-half (Mc._fftshift/
			   fftshift-shift
			   fftshift-identity
			   fftshift-identity
			   fftshift-identity))
(define. Mc.ifftshift-half (Mc._fftshift/
			    fftshift-identity
			    fftshift-identity
			    fftshift-shift
			    fftshift-identity))
(define. Mc.fftshift-complete (Mc._fftshift/
			       fftshift-shift
			       fftshift-shift
			       fftshift-identity
			       fftshift-identity))
(define. Mc.ifftshift-complete (Mc._fftshift/
				fftshift-identity
				fftshift-identity
				fftshift-shift
				fftshift-shift))


(TEST
 > (define tests-complete (list (cons (Mc (Vc 1 -1)
					  (Vc 2 -2)
					  (Vc 3 -3)
					  (Vc 4 -4))
				      (Mc (Vc -3.+0.i 3.+0.i)
					  (Vc -4.+0.i 4.+0.i)
					  (Vc -1.+0.i 1.+0.i)
					  (Vc -2.+0.i 2.+0.i)))
				(cons (Mc (Vc 1 -1)
					  (Vc 2 -2)
					  (Vc 3 -3))
				      (Mc (Vc -3.+0.i 3.+0.i)
					  (Vc -1.+0.i 1.+0.i)
					  (Vc -2.+0.i 2.+0.i)))
				(cons (Mc (Vc 1 -1))
				      (Mc (Vc -1.+0.i 1.+0.i)))
				(cons (Mc (Vc 1 2 3))
				      (Mc (Vc 3.+0.i 1.+0.i 2.+0.i)))))
 > (map (lambda (t)
	  (equal? (Mc.fftshift-complete (car t))
		  (cdr t)))
	tests-complete)
 (#t #t #t #t)
 > (map (lambda (t)
	  (equal? (Mc.ifftshift-complete (cdr t))
		  (car t)))
	tests-complete)
 (#t #t #t #t)
 > (define tests-half (list (cons (Mc (Vc 1 -1)
				      (Vc 2 -2)
				      (Vc 3 -3)
				      (Vc 4 -4))
				  (Mc (Vc 3.+0.i -3.+0.i)
				      (Vc 4.+0.i -4.+0.i)
				      (Vc 1.+0.i -1.+0.i)
				      (Vc 2.+0.i -2.+0.i)))
			    (cons (Mc (Vc 1 -1)
				      (Vc 2 -2)
				      (Vc 3 -3))
				  (Mc (Vc 3.+0.i -3.+0.i)
				      (Vc 1.+0.i -1.+0.i)
				      (Vc 2.+0.i -2.+0.i)))
			    (cons (Mc (Vc 1 -1))
				  (Mc (Vc 1.+0.i -1.+0.i)))
			    (cons (Mc (Vc 1 2 3))
				  (Mc (Vc 1.+0.i 2.+0.i 3.+0.i)))))
 > (map (lambda (t)
	  (equal? (Mc.fftshift-half (car t))
		  (cdr t)))
	tests-half)
 (#t #t #t #t)
 > (map (lambda (t)
	  (equal? (Mc.ifftshift-half (cdr t))
		  (car t)))
	tests-half)
 (#t #t #t #t))


;; the various folds (should tests be near defs?)
(TEST
 > (.fold (Vr 1 2 3) + 0)
 6.
 > (.fold (Vc 1 2 3) + 0)
 6.+0.i
 > (.fold (Mr (Vr 1 2) (Vr 3 4)) + 0)
 10.
 > (.fold (Mc (Vc 1 2) (Vc 3 4)) + 0)
 10.+0.i
 > (.fold (Mc (Vc 1 2) (Vc 3 4+1i)) + 0)
 10.+1.i
 )


;; ============================================================================


(define (vectorlib:type-of v #!optional show-parameters?)
  ;; now now how: extensible, well, isn't, right?.
  ;; NOT fast btw.; and not 'R5RS conform'
  (let ((show (lambda (t . fns)
		(if show-parameters?
		    (cons (symbol-append 'make- t)
			  (map (lambda (fn)
				 (fn v))
			       fns))
		    t))))
    (mcase v
	   (number?
	    (mcase v
		   ((both integer? exact?)
		    (show 'integer (mcase-lambda
				    (fixnum? 'fix)
				    (bignum? 'big))))
		   (rational?
		    (mcase v
			   (exact? 'fractional)
			   (else 'real)))
		   (real? 'real)
		   (complex?
		    ;; (be careful, just ~the rest (what would
		    ;; non-complex numbers be that don't satisfy the
		    ;; complex? predicate?))
		    `(complex ,(vectorlib:type-of (real-part v))
			      ,(vectorlib:type-of (imag-part v))))))
	   (string? (show 'string string-length))
	   (u8vector? (show 'u8vector u8vector-length))
	   (Vr? (show 'Vr .size))
	   (Vc? (show 'Vc .size))
	   (Vi? (show 'Vi .size))
	   (Vs? (show 'Vs .size))
	   (Vb? (show 'Vb .size))
	   (Mr? (show 'Mr .size0 .size1))
	   (Mc? (show 'Mc .size0 .size1))
	   (vector? (show 'vector vector-length)))))

(TEST
 > (vectorlib:type-of 1)
 integer ;; separate into fixnum and bignum ?
 > (vectorlib:type-of 5.)
 real ;; NOT integer unlike what integer? says, ok?
 > (vectorlib:type-of 1.2)
 real
 > (vectorlib:type-of 1.23234234)
 real
 > (vectorlib:type-of 1/2)
 fractional
 > (vectorlib:type-of 5+1i)
 (complex integer integer)
 > (vectorlib:type-of 5.+1i)
 (complex real integer)
 > (vectorlib:type-of 5.+1.i)
 (complex real real)
 > (vectorlib:type-of 5.+1/2i)
 (complex real fractional)
 > (vectorlib:type-of '#(1 2))
 vector
 )


;; type 'with runtime info too' [wl  parametrized types  wl  whatever]
(define (vectorlib:info v)
  (vectorlib:type-of v #t))

(TEST
 > (vectorlib:info 5)
 (make-integer fix) ;; yay
 > (vectorlib:info 5+1/2i)
 (complex integer fractional) ;; hm still; well ok?
 > (vectorlib:info '#(1 2))
 (make-vector 2)
 ;; ^ without saying anything about what it contains; which is ok,
 ;; it's not a homogenous vector. Da.
 > (vectorlib:info '#u8(1 2))
 (make-u8vector 2)
 > (vectorlib:info (Vc 1))
 (make-Vc 1)
 > (vectorlib:info (Mr (Vr 1) (Vr 2)))
 (make-Mr 2 1)
 )

