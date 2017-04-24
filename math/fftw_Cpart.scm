;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 cj-c-util
	 math/vectorlib
	 parallel
	 math/fftw_Cpart-macros)

;; Dependency: apt-get install libfftw3-dev

;; @ means unsafe:
;; @ at the end of an identifier [or before inputs in the type?] means
;; the input is treated unsafely.
;; @ at the start of an identifier means its result is unsafe, i.e. uninitialized.
;; THIS IS DIFFERENT FROM WHAT I USED IN THE PAST.


(c-declare
"
#include <stdlib.h> /* abort */
#include <string.h>
#include \"fftw3.h\"

#define REAL double  /* XXXX choice */

#define VOID2FIX(e) ___CAST(___WORD,(e))
#define FFTWr_BODY(e) ___CAST(REAL*,___BODY(e))
// as fftw type:
#define FFTWC_BODY(e) ___CAST(fftw_complex*,___BODY(e))
// as C-something type:
#include <complex.h>
#define FFTWc_BODY(e) ___CAST(REAL complex*,___BODY(e))

/* TODO: create a library with that?--well, lol. Just some procedure, set a break point in gdb before 'run'ning program. */
void cjabort(void) {
}

")

;; ===  (boxed) scalars
;; move to sep lib? jst for C programming. optimization.
(define (@make-fftwr)
  (@make-realvector 1))
(define (@make-fftwc)
  (@make-realvector 2))
(define (fftwr x)
  (realvector x))
(define (fftwc x y)
  (realvector x y))
(define (->fftwc x)
  (realvector (real-part x)
	      (imag-part x)))
(define-typed (fftwr-> #(fftwr? x))
  (realvector-ref x 0))
(define-typed (fftwc-> #(fftwc? x))
  (+ (realvector-ref x 0)
     (* (realvector-ref x 1) +i)))
(define (fftwr? x)
  (and (realvector? x)
       (= (realvector-length x) 1)))
(define (fftwc? x)
  (and (realvector? x)
       (= (realvector-length x) 2)))
;; operations:

;; definition of define-!fftwcfftwc._ see fftw_Cpart-macros.scm

(define-!fftwcfftwc._ +)
(define-!fftwcfftwc._ -)
(define-!fftwcfftwc._ *)
(define-!fftwcfftwc._ /)

(TEST
 > (define x (fftwc 2. -4.))
 > (define y (fftwc 3. -2.))
 > (define r (@make-fftwc))
 > (!fftwcfftwc.* r x y)
 > r
 #f64(-2. -16.)
 > (!fftwcfftwc.+ r x y)
 > r
 #f64(5. -6.)
 > (!fftwcfftwc.- r x y)
 > r
 #f64(-1. -2.)
 > (begin (!fftwcfftwc./ r x y) r)
 #f64(1.0769230769230769 -.6153846153846155) ;; 14/13-8/13i
 )

;; safe, slow and functional variants:
;; (define-typed (fftwc.* . #((list-of fftwc?) xs))
;;   )

;; or just safe variants.


(define size? (both natural? fixnum?))
(define size0? (both natural0? fixnum?))
;; XXX is this really/always correct? upper bound? (number of words
;; fitting into memory space)
(TEST
 > (size? 0)
 #f
 > (size0? 0)
 #t
 > (size? 1)
 #t
 > (size? -1)
 #f
 > (size0? -1)
 #f
 > (size? 199999999)
 #t
 > (size? 199999999999999999999999)
 #f
 )

;; Represent pointers as fixnums (can be negative!)

;; hm XXX |size?| was meant for words not bytes?
(define-typed (@fftw_malloc #(size? bytes)) 
  (##c-code "___RESULT= VOID2FIX(fftw_malloc (___INT(___ARG1)));" bytes))

(define-typed (fftw_free@ #(fixnum? ptr))
  (##c-code "fftw_free ( ___CAST(void*,___ARG1) );" ptr)
  (void))

(define sizeof-REAL (##c-code "___RESULT= ___FIX(sizeof(REAL));"))
(define sizeof-fftw_complex (##c-code "___RESULT= ___FIX(sizeof(fftw_complex));"))

(define safetymarker #xFEEF8008)
(define sizeof-safetymarker 4) ;; bytes
(c-declare "
#define SAFETYMARKER 0xFEEF8008
#define SIZEOF_SAFETYMARKER 4
")

(define (non-negative-fixnum? v)
  (and (fixnum? v)
       (>= v 0)))

;; bodybytes is excluding the space for the safetymarker; the array at
;; addr must have additional space for sizeof-safetymarker.
(define-typed (set-safetymarker!@ addr #(fixnum? bodybytes))
  (##c-code "
char* p= ___CAST(char*, ___ARG1);
size_t bodybytes= ___INT(___ARG2); //k? range issue
int* pp= ___CAST(int*, &(p[bodybytes]));

*pp= SAFETYMARKER;
" addr bodybytes)
  (void))

;; bodybytes is excluding the space for the safetymarker
(define (safetymarker-ok?@ addr bodybytes)
  (##c-code "
char* p= ___CAST(char*, ___ARG1);
size_t bodybytes= ___INT(___ARG2);
int* pp= ___CAST(int*, &(p[bodybytes]));

___RESULT= (*pp== SAFETYMARKER) ? ___TRU : ___FAL;
" addr bodybytes))


(define (copy-to-body@! v addr lenbytes)
  (##c-code "
char* body= ___CAST(char*, ___BODY(___ARG1));
char* p= ___CAST(char*, ___ARG2);
size_t lenbytes= ___INT(___ARG3);
memcpy(body, p, lenbytes);
" v addr lenbytes)
  (void))

(define (copy-from-body@! addr v lenbytes)
  (##c-code "
char* body= ___CAST(char*, ___BODY(___ARG1));
char* p= ___CAST(char*, ___ARG2);
size_t lenbytes= ___INT(___ARG3);
memcpy(p, body, lenbytes);
" v addr lenbytes)
  (void))


(define-struct. fftwVr
  constructor-name: _fftwVr
  ptr size)

(define-struct. fftwVc
  constructor-name: _fftwVc
  ptr size)

;; size1 increases are adjacent in memory (and viewed as cols or "y"
;; as shown in "2.4 Multi-Dimensional DFTs of Real Data"), size0
;; increases are rows (or "x" as shown in 2.4). See "3.2.1 Row-major
;; Format": "as you step through adjacent memory locations, the first
;; dimension's index varies most slowly and the last dimension's index
;; varies most quickly".

(define-struct. fftwMr
  constructor-name: _fftwMr
  ptr size0 size1)

(define-struct. fftwMc
  constructor-name: _fftwMc
  ptr size0 size1)


(define fftw?
  (either fftwVr?
	  fftwVc?
	  fftwMr?
	  fftwMc?))
(define fftw-vector?
  (either fftwVr?
	  fftwVc?))
(define fftw-2dmatrix?
  (either fftwMr?
	  fftwMc?))

(define-typed (fftw.ptr #(fftw? v))
  (vector-ref v 1))

(define-typed (fftw.freed? #(fftw? v))
  (zero? (vector-ref v 1)))

(define (fftw.freed?-set!@ v)
  (vector-set! v 1 0))

(define (fftw.sizes@ m)
  (values (vector-ref m 2)
	  (vector-ref m 3)))

;; (use of fftw.sizes@ would be faster than fftwMr.size0 etc.)
(define. fftwMr.sizes fftw.sizes@)
(define. fftwMc.sizes fftw.sizes@)


(define-typed (fftw.size #(fftw-vector? v))
  (vector-ref v 2))

(define-typed (fftw.sizes #(fftw-2dmatrix? m))
  (fftw.sizes@ m))

(TEST
 > (define v (@make-fftwVc 3))
 > (fftw.size v)
 3
 > (fftw.free v)
 > (define v (@make-fftwVr 4))
 > (fftw.size v)
 4
 > (fftw.free v)
 > (define m (@make-fftwMr 5 2))
 > (vector-ref (%try-error (fftw.size m)) 1)
 "v does not match fftw-vector?:"
 > (values->vector (fftw.sizes m))
 #(5 2)
 > (fftw.free m)
 )

(define (fftw._free v)
  (let ((ptr (fftw.ptr v)))
    ;; XX multithreading will lead to leak [by some chance, error
    ;; otherwise, so whatever?]
    (fftw.freed?-set!@ v)
    (fftw_free@ ptr)))

(define (fftw.free v)
  (if (fftw.freed? v)
      (error "fftw-free: double free of:" v)
      (fftw._free v)))

;; can't just (define .free fftw.free) since I already have
;; fftwcontext.free
(define. fftwVr.free fftw.free)
(define. fftwVc.free fftw.free)
(define. fftwMr.free fftw.free)
(define. fftwMc.free fftw.free)


;; XX btw I'm not protecting against use after free currently ! all
;; accesses should check that hm

(define (fftw._destroy v)
  (if (fftw.freed? v)
      (void)
      (begin
	(warn "warning: object wasn't freed:" v)
	(fftw._free v))))

(define (@make-fftw-vector_ schemeconstructor sizeof-fftwbasetype)
  (typed-lambda
   (#(size? size))
   (let* ((bodybytes (* sizeof-fftwbasetype size))
	  (lenbytes (+ bodybytes sizeof-safetymarker))
	  (addr (@fftw_malloc lenbytes))
	  (obj (schemeconstructor addr size)))
     (set-safetymarker!@ addr bodybytes)
     (make-will obj fftw._destroy)
     obj)))

(define (@make-fftw-matrix_ schemeconstructor sizeof-fftwbasetype)
  (typed-lambda
   (#(size? x) #(size? y))
   (let* ((size (* x y))
	  (bodybytes (* sizeof-fftwbasetype size))
	  (lenbytes (+ bodybytes sizeof-safetymarker))
	  (addr (@fftw_malloc lenbytes))
	  (obj (schemeconstructor addr x y)))
     (set-safetymarker!@ addr bodybytes)
     (make-will obj fftw._destroy)
     obj)))

;; @ for uninitialized
(define @make-fftwVr (@make-fftw-vector_ _fftwVr sizeof-REAL))
(define @make-fftwVc (@make-fftw-vector_ _fftwVc sizeof-fftw_complex))
(define @make-fftwMr (@make-fftw-matrix_ _fftwMr sizeof-REAL))
(define @make-fftwMc (@make-fftw-matrix_ _fftwMc sizeof-fftw_complex))

(define (fftwVrVr.copy-to-fftw! fr v)
  (let ((size (fftwVr.size fr)))
    (assert (= size (Vr.size v)))
    (##c-code "memcpy(___CAST(void*, ___ARG1),
                      ___BODY(___ARG2),
                      ___INT(___ARG3) * sizeof(REAL)) ;" ;; XX  sizeof(REAL) ok?
	      (fftwVr.ptr fr)
	      v
	      size)
    (void)))

(define (VrfftwVr.copy-from-fftw! v fr)
  (let ((size (fftwVr.size fr)))
    (assert (= size (Vr.size v)))
    (##c-code "memcpy(___BODY(___ARG1),
                      ___CAST(void*, ___ARG2),
                      ___INT(___ARG3) * sizeof(REAL)) ;" ;; XX  sizeof(REAL) ok?
	      v
	      (fftwVr.ptr fr)
	      size)))


;; definition of define-fftw-matrix-ops see fftw_Cpart-macros.scm

(define-fftw-matrix-ops #\r "REAL")
(define-fftw-matrix-ops #\c "REAL complex") ;; XXX instead of fftw_complex so that assignment (see my debugging stuff) works. k?

(TEST
 > (define x (Vr 1 2 3))
 ;; > (define xx (make-fftw-real@ x))
 ;; *** ERROR IN (console)@2.12 -- does not match size?: #f64(1. 2. 3.)
 ;; > (define xx (make-fftw-real@ 4))
 ;; > (VV.copy-to-fftw! xx x)
 ;; *** ERROR IN VV.copy-to-fftw!, "fftw.scm"@97.5 -- assertment failure: (= size (V.size v))
 > (define xx (@make-fftwVr 3))
 > (fftwVrVr.copy-to-fftw! xx x)
 > (define x2 (Vr 9 8 7))
 > (VrfftwVr.copy-from-fftw! x2 xx)
 > x2
 #f64(1. 2. 3.)
 > (fftw.free xx)
 ;; > (fftw.free xx)
 ;; *** ERROR IN (console)@14.1 -- fftw-free: double free of: #(fftw-real #t 3 8275240)

 > (define mm (@make-fftwMr 3 2))
 > (%try-error (fftwMrMr.copy-to-fftw! mm (Mr (Vr 1 2 3) (Vr 4 5 -6) (Vr 7 8 9))))
 #(error "assertment failure: (= s1 s1*)" (= 3 2))
 > (fftwMrMr.copy-to-fftw! mm (Mr (Vr 1. 4.) (Vr 2. 5.) (Vr 3. -6.)))
 > (fftwMr.ref mm 0 0)
 1.
 > (fftwMr.ref mm 0 1)
 4.
 > (fftwMr.ref mm 2 1)
 -6.
 > (define res (@make-Mr 3 2))
 > (MrfftwMr.copy-from-fftw! res mm)
 > res
 #((Mr) 3 2 #(#f64(1. 4.) #f64(2. 5.) #f64(3. -6.)))
 > (fftw.free mm)

 > (define mm (@make-fftwMc 2 3))
 > (fftwMcMc.copy-to-fftw! mm (Mc (Vc 1 2 3) (Vc 4-3i 5 -6)))
 > (vector-ref (%try-error (fftwMr.ref mm 0 0)) 1)
 "m does not match fftwMr?:"
 > (fftwMc.ref mm 0 0)
 1.+0.i
 > (fftwMc.ref mm 1 0)
 4.-3.i
 > (fftwMc.ref mm 1 2)
 -6.+0.i
 > (define res (@make-Mc 2 3))
 > (McfftwMc.copy-from-fftw! res mm)
 > res
 #((Mc) 2 3 #(#((Vc) 3 #f64(1. 0. 2. 0. 3. 0.)) #((Vc) 3 #f64(4. -3. 5. 0. -6. 0.))))
 > (fftw.free mm)
 )

(define-struct. fftw-plan
  ptr)

(define (fftw-plan.freed? v)
  (zero? (fftw-plan.ptr v)))

(define (fftw-plan.freed?-set!@ v)
  (vector-set! v 1 0))

(define. (fftw-plan.free v)
  (if (fftw-plan.freed? v)
      (error "double free")
      (begin
	(##c-code "fftw_destroy_plan(___CAST(fftw_plan,___ARG1));"
		  (fftw-plan.ptr v))
	(fftw-plan.freed?-set!@ v))))

;;(define-typed (fftw-plan- #(fixnum? )))


;; === fftw_plan_dft_1d ===

;;         p = fftw_plan_dft_1d(N, in, out, FFTW_FORWARD, FFTW_ESTIMATE);

(define natural0-fixnum? (both fixnum? natural0?))

(define-typed (fftw_plan_dft_r2c_2d #(fftwMr? in)
				    #(fftwMc? out)
				    #(natural0-fixnum? flags))
  (let-values (((x y) (fftw.sizes@ in))
	       ((x2 y2) (fftw.sizes@ out)))
    (assert (= x x2))
    (assert (= (inc (quotient y 2)) y2))
    (fftw-plan
     (##c-code "___RESULT = VOID2FIX(fftw_plan_dft_r2c_2d
                (___INT(___ARG1), ___INT(___ARG2),
                 ___CAST(REAL*,___ARG3),
                 ___CAST(fftw_complex*,___ARG4),
                 ___INT(___ARG5)));"
	       x y
	       (fftwMr.ptr in)
	       (fftwMc.ptr out)
	       flags))))


(define-typed (fftw_plan_dft_c2r_2d #(fftwMc? in)
				    #(fftwMr? out)
				    #(natural0-fixnum? flags))
  (let-values (((x y) (fftw.sizes@ in))
	       ((x2 y2) (fftw.sizes@ out)))
    (assert (= x x2))
    (assert (= y (inc (quotient y2 2))))
    (fftw-plan
     (##c-code "___RESULT = VOID2FIX(fftw_plan_dft_c2r_2d
                (___INT(___ARG1), ___INT(___ARG2),
                 ___CAST(fftw_complex*,___ARG3),
                 ___CAST(REAL*,___ARG4),
                 ___INT(___ARG5)));"
	       x y2
	       (fftwMc.ptr in)
	       (fftwMr.ptr out)
	       flags))))


;; === fftw_execute ===

;;         fftw_execute(p); /* repeat as needed */

(define-typed (fftw-plan.execute #(fftw-plan? p))
  (##c-code "fftw_execute(___CAST(fftw_plan,___ARG1));" (fftw-plan.ptr p))
  (void))


(define-constant-from-C FFTW_ESTIMATE)
(define-constant-from-C FFTW_MEASURE)

