;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 math/vectorlib
	 parallel
	 math/fftw_Cpart)

(define. (fftwMc.safetymarker-ok? m)
  (letv ((x y) (fftwMc.sizes m))
	(safetymarker-ok?@ (.ptr m)
			   (* x y sizeof-fftw_complex))))

(define. (fftwMr.safetymarker-ok? m)
  (letv ((x y) (fftwMr.sizes m))
	(safetymarker-ok?@ (.ptr m)
			   (* x y sizeof-REAL))))


;;XX move

(define (u32:peek addr numwords)
  (let ((v ;; (make-u32vector (+ numwords 1) 111111)
	 (##make-u32vector (+ numwords 1))))
    (u32vector-set! v numwords       1777777777)
    (u32vector-set! v (dec numwords) 1234567880)
    (copy-to-body@! v addr (* numwords 4))
    (assert (= (u32vector-ref v numwords) 1777777777))
    (assert (/= (u32vector-ref v (dec numwords)) 1234567880))
    (u32vector-shrink! v numwords)
    v))

;; (define (u8:peek addr len)
;;   (let ((v (##make-u8vector (+ len 1))))
;;     (u8vector-set! v len 78)
;;     (u8vector-set! v (dec len) 42)
;;     (copy-to-body@! v addr len)
;;     (assert (= (u8vector-ref v len) 78))
;;     (assert (/= (u8vector-ref v (dec len)) 42))
;;     (u8vector-shrink! v len)
;;     v))

(define (u8:peek addr len)
  (let ((v (##make-u8vector len)))
    (copy-to-body@! v addr len)
    v))

(define-typed (u8:poke addr #(u8vector? v) #!optional len)
  ((typed-lambda (#(size0? len))
		 (copy-from-body@! addr v len))
   (or len (u8vector-length v))))

;;/move


(define-struct. fftwcontext
  in
  out
  plan)
;; use mutation, yes, but use boxes as one should, not mutating pads. fine.

(define. (fftwcontext.free c)
  (.free (.in c))
  (.free (.out c))
  (.free (.plan c)))

(define (.free-boxed b)
  (.free (unbox b)))
(define (.free-boxed* . args)
  (for-each .free-boxed args))


(define-macro* (with-maybe-CTX var form)
  (with-gensyms
   (CTX* ORIG-CTX* RES)
   `(let* ((,ORIG-CTX* ,var)
	   (,var (or ,var (box #f)))
	   (,RES ,form))
      (if (not ,ORIG-CTX*)
	  (.free (unbox ,var)))
      ,RES)))

(define. (Mc:Mr.fft m #!optional ctx*)
  (with-maybe-CTX
   ctx*
   (letv ((x y) (Mr.sizes m))
	 (let* ((outy (inc (quotient y 2)))
		(cont (lambda (ctx)
			(fftwMrMr.copy-to-fftw! (fftwcontext.in ctx) m)
			(fftw-plan.execute (fftwcontext.plan ctx))
			(assert (.safetymarker-ok? (fftwcontext.out ctx)))
			(let ((res (@make-Mc x outy)))
			  (McfftwMc.copy-from-fftw! res (fftwcontext.out ctx))
			  res))))
	   (cond ((unbox ctx*) => cont)
		 (else
		  (let* ((in (@make-fftwMr x y))
			 (out (@make-fftwMc x outy))
			 (plan (fftw_plan_dft_r2c_2d in out FFTW_ESTIMATE))
			 (ctx (fftwcontext in out plan)))
		    (set-box! ctx* ctx)
		    (cont ctx))))))))

;; function to map fft output to 'whole' Mc matrix
(define. (Mc.fft-ref m i0 i1 s1*)
  ;; i0 i1 are indices of the full matrix; s1* is the size of that
  ;; full matrix (i.e. size before the Mc:Mr.fft)
  (letv ((s0 s1) (Mc.sizes m))
	(if (< i1 s1)
	    (Mc.ref m i0 i1)
	    (conj (Mc.ref m
			  (if (zero? i0)
			      i0
			      (- s0 i0))
			  (- s1* i1))))))


(define. (Mc.fftcomplete m s1*)
  (letv ((s0 _s1) (.sizes m))
	(let ((res (Mc:zeros s0 s1*)))
	  (parallel-for-all
	   1000
	   res
	   (s0 s1)
	   (i0 i1)
	   (.set! res i0 i1 (.fft-ref m i0 i1 s1*)))
	  res)))

;; (define. (Mc.fftcomplete m s1*)
;;   (Mc:.map+ m ))


(TEST
 > (.show (.fftcomplete (Mc (Vc 1+1i 2+1i)
			    (Vc 4+1i 5+1i)) 3))
 (Mc (Vc 1.+1.i 2.+1.i 2.-1.i)
     (Vc 4.+1.i 5.+1.i 5.-1.i))
 > (.show (.fftcomplete (Mc (Vc 1+1i 2+1i)
			    (Vc 4+1i 5+1i)
			    (Vc 6+1i 7+1i)) 3))
 (Mc (Vc 1.+1.i 2.+1.i 2.-1.i)
     (Vc 4.+1.i 5.+1.i 7.-1.i)
     (Vc 6.+1.i 7.+1.i 5.-1.i)))


(define. Mr:Mc.ifft
  (typed-lambda
   (m
    #(size? outy)
    #(boolean? normalize?)
    #!optional
    #((maybe box?) ctx*))
   (with-maybe-CTX
    ctx*
    (letv ((x y) (Mc.sizes m))
	  (let* ((cont (lambda (ctx)
			 (fftwMcMc.copy-to-fftw! (fftwcontext.in ctx) m)
			 (fftw-plan.execute (fftwcontext.plan ctx))
			 (assert (.safetymarker-ok? (fftwcontext.out ctx)))
			 (let ((res (@make-Mr x outy)))
			   (MrfftwMr.copy-from-fftw! res (fftwcontext.out ctx))
			   (if normalize?
			       (Mr../ res (* x outy))
			       res)))))
	    (cond ((unbox ctx*) => cont)
		  (else
		   (let* ((in (@make-fftwMc x y))
			  (out (@make-fftwMr x outy))
			  (plan (fftw_plan_dft_c2r_2d in out FFTW_ESTIMATE))
			  (ctx (fftwcontext in out plan)))
		     (set-box! ctx* ctx)
		     (cont ctx)))))))))


;;XX should really namespace these.
(define (c/ctx fn)
  (let* ((c (box #f))
	 (res (fn c)))
    (.free (unbox c))
    res))
(define-macro* (CTX expr)
     `(c/ctx (lambda (ctx) ,expr)))

(TEST
 > (define ctx-3*3 (box #f))
 > (define res3*3 (Mc:Mr.fft (Mr (Vr 1 2 3) (Vr 4 5 -6) (Vr 7 8 9)) ctx-3*3))
 > (.show res3*3)
 (Mc (Vc 33.+0.i
	 1.5-7.794228634059947i)
     (Vc -7.5+18.186533479473212i
	 -12.-8.881784197001252e-16i)
     (Vc -7.5-18.186533479473212i
	 6.+10.392304845413264i))
 > (CTX (.show (Mc:Mr.fft (Mr (Vr 1)) ctx)))
 (Mc (Vc 1.+0.i))
 > (CTX (.show (Mc:Mr.fft (Mr (Vr 1 2)) ctx)))
 (Mc (Vc 3.+0.i -1.+0.i))
 > (CTX (.show (Mc:Mr.fft (Mr (Vr 1 2 3)) ctx)))
 (Mc (Vc 6.+0.i -1.5+.8660254037844386i))
 > (define ctx-2*2 (box #f))
 > (.show (Mc:Mr.fft (Mr (Vr 1 2) (Vr 3 4)) ctx-2*2))
 (Mc (Vc 10.+0.i -2.+0.i) (Vc -4.+0.i 0.+0.i))
 > (.show (Mc:Mr.fft (Mr (Vr 1 2) (Vr 3 -4)) ctx-2*2))
 (Mc (Vc 2.+0.i 6.+0.i) (Vc 4.+0.i -8.+0.i))
 > (define res129342 (CTX (Mc:Mr.fft (Mr (Vr 1 2 0 3 4 2)) ctx)))
 > (.show res129342)
 (Mc (Vc 12.+0.i
	 -2.+3.4641016151377544i
	 0.-3.4641016151377544i
	 -2.+0.i))
 > (define res129342b (CTX (Mc:Mr.fft
			    (Mr (Vr 1) (Vr 2) (Vr 0) (Vr 3) (Vr 4) (Vr 2))
			    ctx)))
 > (.show res129342b)
 (Mc (Vc 12.+0.i)
     (Vc -2.+3.4641016151377544i)
     (Vc 0.-3.4641016151377544i)
     (Vc -2.+0.i)
     (Vc 0.+3.4641016151377544i)
     (Vc -2.-3.4641016151377544i)
     )
 > (CTX (.show (Mc:Mr.fft (Mr (Vr 1) (Vr 2)) ctx)))
 (Mc (Vc 3.+0.i) (Vc -1.+0.i))
 > (.free-boxed* ctx-3*3 ctx-2*2)
 )

(TEST
 > (.show (CTX (Mr:Mc.ifft (Mc (Vc 6.+0.i -1.5+.8660254037844386i)) 3 #f ctx)))
 (Mr (Vr 3. 6. 9.))
 > (.show (CTX (Mr:Mc.ifft res129342 6 #f ctx)))
 (Mr (Vr 6. 12. 1.7763568394002505e-15 18. 24. 12.))
 > (.show (CTX (Mr:Mc.ifft res129342b 1 #f ctx)))
 (Mr (Vr 6.) (Vr 12.) (Vr 1.7763568394002505e-15) (Vr 18.) (Vr 24.) (Vr 12.))
 ;; or normalized:
 > (.show (CTX (Mr:Mc.ifft res129342b 1 #t ctx)))
 (Mr (Vr 1.) (Vr 2.) (Vr 2.9605947323337506e-16) (Vr 3.) (Vr 4.) (Vr 2.))
 > (.show (CTX (Mr:.ifft res3*3 3 #t ctx)))
 (Mr (Vr 1. 2. 3.) (Vr 4. 5. -6.) (Vr 7. 8. 9.))
 )
