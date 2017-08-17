;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (cj-match mcase)
	 cj-symbol
	 test
	 ;; ?
	 C
	 (cj-env-1))

(declare (block)(standard-bindings)(extended-bindings))



;; that which cj-standarddeclares.scm was.
;; no args yet, perhaps later..
;; (define-macro* (cj-declare)
;;  `(declare (block)(standard-bindings)(extended-bindings)))
;;stupid, doesn't work (macro output not looked at in right phase by compiler?)


;; stop hand-rolling these 0..n-1 loops
(define-macro* (for..< var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensyms
	   (LP)
	   `(let ,LP ((,var ,from))
		 (if (fx< ,var ,to)
		     (begin
		       ,@body
		       (,LP (fx+ ,var 1)))))))))

(define-macro* (for..<* var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensyms
	   (LP TO)
	   `(let ((,TO ,to))
	      (assert (fixnum? ,TO))
	      (let ,LP ((,var ,from))
		   (if (##fixnum.< ,var ,TO)
		       (begin
			 ,@body
			 (,LP (##fixnum.+ ,var 1))))))))))

(TEST
 > (let ((v (make-vector 5))) (for..< (i 0 5) (vector-set! v i (* i i))) v)
 #(0 1 4 9 16)
 > (let ((v (make-vector 5))) (for..<* (i 0 5) (vector-set! v i (* i i))) v)
 #(0 1 4 9 16)
 )


;; move to where?
(define (current-unixtime)
  (inexact->exact (floor (time->seconds (current-time)))))


(define-macro* (xcase expr . cases)
  (with-gensym
   V
   `(let ((,V ,expr))
      (case ,V
	,@cases
	(else ;; #t doesn't work for case, only for cond
	 (error "no match for:" ,V))))))

(TEST
 > (xcase 2 ((3) 'gut) ((2) 'guttoo))
 guttoo
 > (%try-error (xcase 1 ((3) 'gut) ((2) 'guttoo)))
 #(error "no match for:" 1)
 )

(define-macro* (xcond . cases)
  `(cond ,@cases
	 (else (error "no match"))))


;; does that really warrant a persistent name?
;; [could almost just use for..<,too?]
(define-macro* (repeat n . body)
  (with-gensyms
   (LP C)
   `(let ,LP ((,C ,n)
	      (res (void)))
	 (if (positive? ,C)
	     (,LP (dec ,C)
		  (begin
		    ,@body))
	     res))))

(define-macro* (unless test form)
  `(if (not ,test)
       ,form))

(define -e file-exists?)

(define (append-newline str)
  (string-append str "\n"))

(define-macro* (future expr)
  `(thread-start!
    (make-thread
     (lambda ()
       ,expr)
     'future)))

(define (future? v)
  (and (thread? v)
       (eq? (thread-name v) 'future)))
;; there's no ##thread-name, sigh

;; future-value, future-join (, future-wait)
(define @future-force thread-join!)

(define (future-force v)
  (if (future? v)
      (@future-force v)
      (error "not a future:" v)))

(TEST
 > (define f (future 12))
 > (define g (thread-start! (make-thread (lambda () 13))))
 > (future? f)
 #t
 > (future? g)
 #f
 > (@future-force f)
 12
 > (@future-force g)
 13
 > (future-force f)
 12
 > (with-exception-catcher error-exception-message (lambda () (future-force g)))
 "not a future:")



(define-macro* (CA . forms)
  (let* ((_? (lambda (v)
	       (eq? (source-code v) '_)))
	 (forms* (map (lambda (form)
			(if (_? form)
			    (cons #t (gensym))
			    (cons #f form)))
		      forms))
	 (vs (map cdr (filter car forms*))))
    (with-gensym
     R
     `(lambda ,(if* vs `(,@vs . ,R) R)
	(apply ,@(map cdr forms*) ,R)))))

(TEST
 > ((CA vector) 1 2)
 #(1 2)
 > ((CA vector _ '+) 1 2)
 #(1 + 2)
 > (define TEST:equal? syntax-equal?)
 > (expansion#CA vector _ '+)
 (lambda (GEN:-5527 . GEN:R-5528) (apply vector GEN:-5527 '+ GEN:R-5528))
 > (expansion#CA vector '+)
 (lambda GEN:R-5529 (apply vector '+ GEN:R-5529)))

