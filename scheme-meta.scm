;;; Copyright 2016 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Meta-knowledge about Scheme

(require ;;(cj-functional pair-of)  circular
	 ;;(vector-util vector-of)  circular
 test)


(export void?
	optional?
	key?
	rest?
	
	self-quoting?
	constant-expr?
	perhaps-quote
	uvector?
	svector?
	fvector?
	homogenous-vector?
	sexpr-object?
	sexpr?

	#!optional
	pair-of-sexpr?
	vector-of-sexpr?
	void?)

;; COPY to avoid circular dep -----  XX move here instead?
(##namespace ("scheme-meta#" inc vector-every vector-of))

(define inc (##lambda (n) (+ n 1)))

(define vector-every
  (##lambda
   (t? v)
   (let ((len (vector-length v)))
     (let lp ((i 0))
       (if (< i len) (and (t? (vector-ref v i)) (lp (inc i))) #t)))))

(define vector-of
  (##lambda (t?) (lambda (v) (and (vector? v) (vector-every t? v)))))

;; /COPY

(define (void? v)
  ;; (eq? v (void))  or, since I don't have a void.show yet either:
  (eq? v #!void))

(define (optional? v)
  (eq? v #!optional))

(define (key? v)
  (eq? v #!key))

(define (rest? v)
  (eq? v #!rest))


(define (self-quoting? v)
  ;; avoid depending on |either| from cj-functional ?
  (or (string? v)
      (number? v)
      (boolean? v)
      (char? v)
      (eof-object? v)
      (void? v)
      (optional? v)
      (key? v)
      (rest? v)
      (keyword? v)))


;; XX also check quasiquote with only constants used inside?
;; Or...etc.. deep program analysis? I.e. there may be many more
;; things that are constant, without this returning #t for it.
(define (constant-expr? v)
  (or (self-quoting? v)
      (and (pair? v)
	   (eq? (source-code (car v)) 'quote)
	   (pair? (cdr v))
	   (null? (cddr v)))))

(TEST
 > (constant-expr? 10)
 #t
 > (constant-expr? ''v)
 #t
 > (constant-expr? (list 'quote 'v 'w))
 #f)


(define (perhaps-quote v)
  (if (self-quoting? v)
      v
      (list 'quote v)))



(define (uvector? v)
  (or (u8vector? v)
      (u16vector? v)
      (u32vector? v)))

(define (svector? v)
  (or (s8vector? v)
      (s16vector? v)
      (s32vector? v)))

(define (fvector? v)
  (or (f32vector? v)
      (f64vector? v)))

(define (homogenous-vector? v)
  (or (uvector? v)
      (svector? v)
      (fvector? v)))

;; XX bit vectors ?


(define (sexpr-object? v)
  (or (symbol? v)
      (self-quoting? v)
      (null? v)
      (pair? v)
      (vector? v)
       ;; XX really?
      (homogenous-vector? v)))


(define (sexpr? v)
  (or (symbol? v)
      (self-quoting? v)
      (null? v)
      (pair-of-sexpr? v)
      (vector-of-sexpr? v)
      ;; XX really?
      (homogenous-vector? v)
      (box? v)))

(define pair-of-sexpr? (let ()
			 ;; COPY from cj-functional to resolve
			 ;; dependency cycle
			 (define (pair-of t1? t2?)
			   (lambda (v)
			     (and (pair? v)
				  (t1? (car v))
				  (t2? (cdr v)))))
			 (pair-of sexpr? sexpr?)))
(define vector-of-sexpr? (vector-of sexpr?))


'(TEST
 ;; hum, stupidly, is the only data structure not supported
 ;; procedures, continuations, and other Gambit data structures
 ;; (cj-struct ones would be considered s-expressions)? If Gambit used
 ;; the cj-struct approach then there would be *none*? ! Wow
 ;; beauty. (Also, interestingly, Gambit does use procedure values in
 ;; its expanded source code, but ok, those are not sexprs anymore.)
 > (sexpr? cons)
 #f
 ;; Notably also: (wow, that removes the need for |source-sexpr?|)
 > (sexpr? (quote-source 4))
 #t
 )

