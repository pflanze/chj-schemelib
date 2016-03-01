;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-phasing
	 cj-inline-1
	 test
	 ;; require this?: (understand it as part of bundle?)
	 cj-env-1)

;; various small bits that I don't know where to put

;; (include "cj-env-1.scm") already in test.scm

(define (symbol-or-string->string v)
  (cond ((string? v)
	 v)
	((symbol? v)
	 (symbol->string v))
	(else (error "invalid type:" v))))

(define (symbol-append . vals)
  (string->symbol
   (apply string-append
	  (map 
	   symbol-or-string->string
	   vals))))


;; a compile time if:
(define-macro* (IF test yes #!optional (no '(begin)))
  (if (eval test)
      yes
      no))


;; an if that only accepts booleans:

(define-macro* (If test yes no)
  (let ((V (gensym 'v)))
    `(##let ((,V ,test))
	    (##if (##eq? ,V #t)
		  ,yes
		  (##if (##eq? ,V #f)
			,no
			(If-error ,V))))))

(define (If-error v)
  (error "If: expecting boolean, got:" v))


(define-macro* (named-lambda name vars . body)
  `(letrec ((,name (lambda ,vars
		     ,@body)))
     ,name))


(both-times
 (define (natural? x)
   (and (integer? x)
	(positive? x))))

(define (natural0? x)
  (and (integer? x)
       (not (negative? x))))

(define (positive-real? x)
  (and (real? x)
       (positive? x)))


;; move to some other lib?

;; 'Machine' integer types (predicates)

(define (uint_? bits)
  (let ((ma (dec (arithmetic-shift 1 bits))))
    (lambda (x)
      (and (integer? x)
	   (<= 0 x ma)))))

(define uint8? (uint_? 8))
(define uint16? (uint_? 16))
(define uint32? (uint_? 32))
(define uint64? (uint_? 64))
(define uint128? (uint_? 128))

(define (int_? bits)
  (let ((ma (dec (arithmetic-shift 1 (dec bits))))
	(mi (- (arithmetic-shift 1 (dec bits)))))
    (lambda (x)
      (and (integer? x)
	   (<= mi x ma)))))

(define int8? (int_? 8))
(define int16? (int_? 16))
(define int32? (int_? 32))
(define int64? (int_? 64))
(define int128? (int_? 128))

(TEST
 > (define (t _)
     (map _
	  (list 0 1 -1 127 128 -127 -128 -129
		255 256 -255 -256
		18446744073709551615
		18446744073709551616
		-9223372036854775808
		-9223372036854775809)))
 > (t uint8?)
 (#t #t #f #t #t #f #f #f #t #f #f #f #f #f #f #f)
 > (t int8?)
 (#t #t #t #t #f #t #t #f #f #f #f #f #f #f #f #f)
 > (t uint64?)
 (#t #t #f #t #t #f #f #f #t #t #f #f #t #f #f #f)
 > (t int64?)
 (#t #t #t #t #t #t #t #t #t #t #t #t #f #f #t #f))

;; /move

(both-times
 (define (make-list n v)
   (let lp ((n n)
	    (res '()))
     (if (positive? n)
	 (lp (dec n)
	     (cons v res))
	 res))))

(define-macro* (first-then arity* access cmp)
  (let ((arity (eval arity*)))
    (if (natural? arity)
	(let ((ACCESS (gensym 'access))
	      (VARS (map gensym (make-list arity 'v))))
	  `(let ((,ACCESS ,access))
	     (lambda ,VARS
	       (,cmp ,@(map (lambda (V)
			      `(,ACCESS ,V))
			    VARS))))))))

(define-macro* (on access cmp)
  `(first-then 2 ,access ,cmp))

;; +- the same code that is in define-macro*.scm (using path-directory)
;; Can't unify because of bootstrapping, right?
(define-macro* (path-normalize/origin=source path)
  (let* ((loc (source-location path))
	 (container (container->path (location-container loc)))
	 (path (source-code path)))
    (or (string? container)
	(error "path-normalize/origin=source: only works in files, not:" container))
    (let ((sourcedir
	   (path-directory container)))
      `(path-normalize ,path #f ,sourcedir))))


;; n-ary |equal?|
(define (equal?* . vs)
  ;; don't use let-pair yet (bootstrap)
  (let ((v1 (car vs))
	(vs (cdr vs)))
    (let lp ((vs vs))
      (if (null? vs)
	  #t
	  (let ((v (car vs))
		(vs (cdr vs)))
	    (and (equal? v1 v)
		 (lp vs)))))))
(TEST
 > (equal?* 2 2 2)
 #t
 > (equal?* 2 2 3)
 #f
 > (equal?* 2 3 2)
 #f
 > (equal?* 3 2 2)
 #f
 )


(define-macro* (thunk . body)
  `(lambda ()
     ,@body))


;; |export| is for documentary purposes only currently
(define-macro* (export . forms)
  '(begin))


(define (box-inc! b)
  (let ((v (inc (unbox b))))
    (set-box! b v)
    v))
(define (box-dec! b)
  (let ((v (dec (unbox b))))
    (set-box! b v)
    v))

(TEST
 > (def b (box 10))
 > b
 #&10
 > (box-inc! b)
 11
 > b
 #&11
 > (box-dec! b)
 10
 > b
 #&10)

(define-macro* (inc! v)
  (let ((V (gensym)))
    `(let ((,V (inc ,v)))
       (set! ,v ,V)
       ,V)))

(define-macro* (dec! v)
  (let ((V (gensym)))
    `(let ((,V (dec ,v)))
       (set! ,v ,V)
       ,V)))


;; treating null as false:

(define-inline (not* v)
  (or (eq? v #f)
      (eq? v '())))

(define-macro* (if* test yes #!optional no)
  `(if (##not (not* ,test))
       ,yes
       ,@(if no ;; careful; relying on source code wrapping so that
		;; the code `#f will be true here
	     (list no)
	     '())))

(TEST
 > (if* #t 1 2)
 1
 > (if* #f 1 2)
 2
 > (if* '() 1 2)
 2
 > (if* #f 1 #f)
 #f
 )

;; Simpler than named-lambda and combinable with other (special) forms:

(define-macro* (named name form)
  (assert* symbol? name
	   (lambda (_)
	     `(letrec ((,name ,form))
		,name))))


(define (pp-through a . r)
  (define port (current-error-port))
  (if (pair? r)
      (if (null? (cdr r))
	  (begin
	    (display a port)
	    (display ":" port)
	    (newline port)
	    (pretty-print (car r) port)
	    (car r))
	  (error "too many arguments"))
      (begin
	(pretty-print a port)
	a)))

(define (no-pp-through a . r)
  (if (pair? r)
      (if (null? (cdr r))
	  (car r)
	  (error "too many arguments"))
      a))


;; (define number->integer ;; any better name?
;;   (compose inexact->exact floor))
;;dependency issue, compose is in cj-functional

(define (number->integer x) ;; any better name?
  (inexact->exact (floor x)))


(define (force-thunk th)
  (th))


(define-macro* (lambda_ . body)
  `(lambda (_)
     ,@body))

(TEST
 > ((lambda_ (and (deadlock-exception? _) "ok")) 1)
 #f
 > ((lambda_ (and (string? _) "ok")) 1)
 #f
 > ((lambda_ (and (string? _) "ok")) "")
 "ok"
 )


;; (futures, mostly meant for easier debugging)
(define-macro* (fut . body)
  `(thread-start! (make-thread (thunk ,@body))))

(define (fjoin th)
  (thread-join! th))


(define (identity x)
  x)


(define (symbol-value-or sym thunk)
  (with-exception-catcher
   (lambda (e)
     (if (unbound-global-exception? e)
	 (thunk)
	 (raise e)))
   (lambda ()
     (eval sym))))

(define (thunk-symbol-value-or sym-thunk or-thunk)
  (with-exception-catcher
   (lambda (e) (if (unbound-global-exception? e) (or-thunk) (raise e)))
   (lambda ()
     (let ((v (sym-thunk)))
       (if (##unbound? v)
	   (or-thunk)
	   v)))))

(define-macro* (macro-symbol-value-or sym thunk)
  (if (symbol? (source-code sym))
      `(thunk-symbol-value-or
	(lambda () ,sym)
	,thunk)
      (source-error sym "not a symbol")))

(define-macro* (define-if-not-defined name expr)
  (assert* symbol? name
	   (lambda (name)
	     `(define ,name
		(thunk-symbol-value-or (lambda () ,name)
				       (lambda () ,expr))))))

(TEST
 > (define-if-not-defined abczxfwef 10)
 > abczxfwef
 10
 > (define-if-not-defined abczxfwef 11)
 > abczxfwef
 10
 )

(define-if-not-defined gambit:load load)

;; a cleaner interface?:

;; (Others would just use a Maybe type and no contortions with
;; continuations would be needed.)

(define (*if-symbol-value sym *then *else)
  (continuation-capture
   (lambda (cont)
     (with-exception-catcher
      (lambda (e)
	(if (unbound-global-exception? e)
	    (*else)
	    (raise e)))
      (lambda ()
	(continuation-graft
	 cont
	 *then
	 (eval sym)))))))

(TEST
 > (*if-symbol-value 'woiuewfoiu
		     vector
		     (lambda () 'no))
 no
 > (*if-symbol-value '*if-symbol-value
		     (lambda (v) (eq? v *if-symbol-value))
		     (lambda () 'no))
 #t
 > (with-exception-catcher
    unbound-global-exception-variable
    (lambda ()
      (*if-symbol-value '*if-symbol-value
			(lambda (v) xh68zzn3j5mc9p2tu2q)
			(lambda () 'no))))
 xh68zzn3j5mc9p2tu2q)


(define table-update!:noval (box 'noval))
(define (table-update! t k fn
		       #!optional
		       ;; *not* called in tail pos!
		       (notexist (lambda ()
				   (error "key not found"))))
  (let ((v (table-ref t k table-update!:noval)))
    (table-set! t k
		(if (eq? v table-update!:noval)
		    (notexist)
		    (fn v)))))

(TEST
 > (define t (make-table))
 > (%try-error (table-update! t 'a inc))
 #(error "key not found")
 > (table-set! t 'a 1)
 > (table-update! t 'a inc)
 > (table-ref t 'a)
 2
 > (table-update! t 'b inc (lambda () 10))
 > (table-ref t 'b)
 10)


(define-macro* (define-parameter name default-value)
  `(define ,name (make-parameter ,default-value)))

