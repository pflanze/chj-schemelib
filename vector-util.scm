;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
	 (fixnum inc)
	 ;; (cj-env-2 for..<), no, to avoid cycle
         (cj-match mcase)
         (simple-match assert*)
         (cj-symbol-with with-gensyms)
         (list-util-1 map/iota)
	 test
	 ;; vector-util-1 ;; well, cj-source or mod/mod.scm since those include it?
	 srfi-1
	 (list-util-1 map/iota)
         cj-phasing
         (cj-symbol syntax-equal?))


(export vectors-map
	vector-map
	vector-map*
	;; note: dot-oo versions of these are now also in oo-vector-lib
	vector-fold-right
	vector-fold
	vector-for-each
	vector-generate
	if-avector-ref
	maybe-avector-ref
	vector-every
	vector-of
	vector-of/length
	;; note: not oo, just curried:
	vector.value.pos
        apply-vector
        apply-vector/arity
        ;; also @apply-vector-0 etc. as well as apply-vector-0 etc.?
        (macro let-vector))


(include "cj-standarddeclares.scm")


(define (vectors-map fn vecs accept-uneven-lengths?)
  (let* ((lens (map vector-length vecs))
	 (len (apply min lens))
	 (cont (lambda ()
		 (let* ((res (make-vector len)))
		   (let lp ((i 0))
		     (if (= i len)
			 res
			 (begin
			   (vector-set! res
					i
					(apply fn
					       (map (lambda (vec)
						      (vector-ref vec i))
						    vecs)))
			   (lp (inc i)))))))))
    (if accept-uneven-lengths?
	(cont)
	(let ((lenmax (apply max lens)))
	  (if (= len lenmax)
	      (cont)
	      (error "uneven lengths of input vectors (min max):"
		     len lenmax))))))

(define (vector-map fn . vecs)
  (vectors-map fn vecs #f))

(define (vector-map* fn . vecs)
  (vectors-map fn vecs #t))

(TEST
 > (vector-map + '#(1 2 3) '#(3 4 5))
 #(4 6 8)
 > (with-exception-catcher error-exception-message (lambda () (vector-map + '#(1 2 3) '#(3 4))))
 "uneven lengths of input vectors (min max):"
 > (vectors-map + '(#(1 2 3) #(3 4)) #t)
 #(4 6)
 > (vector-map* + '#(1 2 3) '#(3 4))
 #(4 6)
 )


;; NOTE: dot-oo versions of these are now also in oo-vector-lib
(define (vector-fold-right fn tail vec)
  (let ((len (vector-length vec)))
    (let rec ((i 0))
      (if (= i len)
	  tail
	  (fn (vector-ref vec i)
	      (rec (inc i)))))))

(define (vector-fold fn tail vec)
  (let ((len (vector-length vec)))
    (let lp ((res tail)
	     (i 0))
      (if (= i len)
	  res
	  (lp (fn (vector-ref vec i)
		  res)
	      (inc i))))))

(TEST
 > (fold vector 'null '(1 2 3))
 #(3 #(2 #(1 null)))
 > (vector-fold vector 'null '#(1 2 3))
 #(3 #(2 #(1 null)))
 > (fold-right vector 'null '(1 2 3))
 #(1 #(2 #(3 null)))
 > (vector-fold-right vector 'null '#(1 2 3))
 #(1 #(2 #(3 null)))
 )

(define (vector-for-each proc vec)
  (vector-fold (lambda (v _)
		 (proc v))
	       #f
	       vec))

;; /oo-vector-lib


;; (define (vector-unfold end? fmap fnext start)
;;   )
;; has the problem of not knowing how many elements it will contain in advance.
;; hence rather: vector-generate

(define (vector-generate size fn/i)
  (let ((res (make-vector size)))
    (let lp ((i 0))
      (when (< i size)
	    (vector-set! res i (fn/i i))
	    (lp (inc i))))
    res))

(TEST
 > (vector-generate 10 square)
 #(0 1 4 9 16 25 36 49 64 81)
 )


;; (define (vector/list.for-each proc/i+V+L starti endi v l)
;;   ;; (for..< (i starti endi)
;;   ;; 	  )
;;   (let lp ((i starti)
;; 	   (l l))
;;     (if (< i endi)
;; 	(begin
;; 	  (proc/V+L i
;; 		    (vector-ref v i)
;; 		    (car l))
;; 	  (lp (inc i)
;; 	      (cdr l))))))


;; association vectors: vectors of some kind of tuple; access is the
;; analogon of a car of an alist (could be car, of course; but structs
;; more 'common'?)

(define (if-avector-ref vec starti maybe-endi access equal? val
			then/i+tuple else/0)
  (let ((endi (or maybe-endi (vector-length vec))))
    ;; can't use for..< here because of not having a 'return'.
    (let lp ((i starti))
      (if (< i endi)
	  (let ((tupl (vector-ref vec i)))
	    (if (equal? (access tupl) val)
		(then/i+tuple i tupl)
		(lp (inc i))))
	  (else/0)))))

(define (maybe-avector-ref vec starti maybe-endi access equal? val)
  (if-avector-ref vec starti maybe-endi access equal? val
		  (lambda (i tuple)
		    tuple) false/0))

(TEST
 > (maybe-avector-ref '#(foo (a 1) (c 2) (b 10)) 1 #f car eq? 'c)
 (c 2)
 > (maybe-avector-ref '#(foo (a 1) (c 2) (b 10)) 1 #f car eq? 'd)
 #f)



;; Also see definition in oo-vector-lib.scm
(define (vector-every t? v)
  (let ((len (vector-length v)))
    (let lp ((i 0))
      (if (< i len)
	  (and (t? (vector-ref v i))
	       (lp (inc i)))
	  #t))))


(define (vector-of t?)
  (lambda (v)
    (and (vector? v)
	 (vector-every t? v))))

;; vgl. u8vector-of-length, but here we want a type check as well
(define (vector-of/length t? len)
  (lambda (v)
    (and (vector? v)
	 (= (vector-length v) len)
	 (vector-every t? v))))

(TEST
 > (map (vector-of integer?)
	'(#() #(#f) #(#t) 1
	   #(a)
	   #(1 2 3)
	   #(1 2 a)
	   #(a 2 3)
	   #(1 a 3)))
 (#t #f #f #f
     #f #t #f #f #f))



(define vector-util:no-alternative (gensym "no-alternative"))

(define (vector.value.pos vs)
  (let ((t (list->table (map/iota (lambda (s i)
				    (cons s i))
				  (vector->list vs)))))
    (lambda (k #!optional (alternative vector-util:no-alternative))
      (let ((v (table-ref t k alternative)))
	(if (eq? v vector-util:no-alternative)
	    (error "unknown key:" k)
	    v)))))



(define (@apply-vector-0 f v) (f))
(define (@apply-vector-1 f v) (f (##vector-ref v 0)))
(define (@apply-vector-2 f v) (f (##vector-ref v 0) (##vector-ref v 1)))
(define (@apply-vector-3 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2)))
(define (@apply-vector-4 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3)))
(define (@apply-vector-5 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4)))
(define (@apply-vector-6 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4) (##vector-ref v 5)))
(define (@apply-vector-7 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4) (##vector-ref v 5) (##vector-ref v 6)))
(define (@apply-vector-8 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4) (##vector-ref v 5) (##vector-ref v 6) (##vector-ref v 7)))
(define (@apply-vector-9 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4) (##vector-ref v 5) (##vector-ref v 6) (##vector-ref v 7) (##vector-ref v 8)))
(define (@apply-vector-10 f v) (f (##vector-ref v 0) (##vector-ref v 1) (##vector-ref v 2) (##vector-ref v 3) (##vector-ref v 4) (##vector-ref v 5) (##vector-ref v 6) (##vector-ref v 7) (##vector-ref v 8) (##vector-ref v 9)))

(define (apply-vector-arity-error arity val)
  (error (string-append "apply-vector-"
                        (number->string arity)
                        ": wrong length of vector: ")
         val))

(insert-result-of
 `(begin
    ,@(map (lambda (i)
             `(define (,(symbol-append 'apply-vector- (number->string i)) f v)
                (if (= (vector-length v) ,i)
                    (,(symbol-append '@apply-vector- (number->string i)) f v)
                    (apply-vector-arity-error ,i v))))
           (iota 11))))


(define (apply-vector f v)
  (let ((len (vector-length v)))
    (case len
      ((0) (@apply-vector-0 f v))
      ((1) (@apply-vector-1 f v))
      ((2) (@apply-vector-2 f v))
      ((3) (@apply-vector-3 f v))
      ((4) (@apply-vector-4 f v))
      ((5) (@apply-vector-5 f v))
      ((6) (@apply-vector-6 f v))
      ((7) (@apply-vector-7 f v))
      ((8) (@apply-vector-8 f v))
      ((9) (@apply-vector-9 f v))
      ((10) (@apply-vector-10 f v))
      (else
       (apply f (vector->list v))))))


(define (apply-vector/arity arity)
  (case arity
    ((0) apply-vector-0)
    ((1) apply-vector-1)
    ((2) apply-vector-2)
    ((3) apply-vector-3)
    ((4) apply-vector-4)
    ((5) apply-vector-5)
    ((6) apply-vector-6)
    ((7) apply-vector-7)
    ((8) apply-vector-8)
    ((9) apply-vector-9)
    ((10) apply-vector-10)
    (else
     (error "apply-vector/arity: not defined for arity:" arity))))


(define (let-vector:error-need-got need got)
     (error "let-vector: need vector of length, but got:" need got))

(define-macro* (let-vector bindform . body)
  (mcase bindform
         (`(`vars `expr)
          (assert* list? vars
                   (lambda (vars)
                     (with-gensyms
                      (vec len)
                      `(let* ((,vec ,expr)
                              (,len (vector-length ,vec)))
                         (if (= ,len ,(length vars))
                             (let ,(map/iota (lambda (var i)
                                               `(,var (vector-ref ,vec ,i)))
                                             vars)
                               ,@body)
                             (let-vector:error-need-got ,(length vars)
                                                        ,len)))))))))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#let-vector ((a b) (vector 5 6)) (/ a b))
 (let* ((GEN:vec-6925 (vector 5 6))
        (GEN:len-6926 (vector-length GEN:vec-6925)))
   (if (= GEN:len-6926 2)
       (let ((a (vector-ref GEN:vec-6925 0))
             (b (vector-ref GEN:vec-6925 1)))
         (/ a b))
       (let-vector:error-need-got 2 GEN:len-6926)))
 > (let-vector ((a b) (vector 5 6)) (/ a b))
 5/6
 > (%try (let-vector ((a b) (vector 5 6 7)) (/ a b)))
 (exception text: "let-vector: need vector of length, but got: 2 3\n")
 > (%try (let-vector ((a b) (vector 5)) (/ a b)))
 (exception text: "let-vector: need vector of length, but got: 2 1\n")
 > (let-vector (() (vector)) 'ok)
 ok)

