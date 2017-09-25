;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-phasing
	 cj-inline-1
	 test
	 ;; require this?: (understand it as part of bundle?)
	 cj-env-1
	 symbol-append
	 (cj-source show-procedure-location))

(declare (block)(standard-bindings)(extended-bindings))


;; various small bits that I don't know where to put

;; (include "cj-env-1.scm") already in test.scm


;; a compile time if:
(define-macro* (IF test yes #!optional (no '(begin)))
  (if (eval test)
      yes
      no))


;; an if that only accepts booleans:

(define-macro* (If test yes #!optional no)
  (let ((V (gensym 'v)))
    `(##let ((,V ,test))
	    (##if (##eq? ,V #t)
		  ,yes
		  ,@(if no
			`((##if (##eq? ,V #f)
				,no
				(If-error ,V)))
			'())))))

(define (If-error v)
  (error "If: expecting boolean, got:" v))


(both-times
 (define (natural? x)
   (and (integer? x)
	(positive? x))))

;; XX move to predicates.scm ?:

(define (natural0? x)
  (and (integer? x)
       (not (negative? x))))

(define (fxnatural? v)
  (and (fixnum? v)
       (fxpositive? v)))

(define (fxnatural0? v)
  (and (fixnum? v)
       (not (fxnegative? v))))

(define (positive-real? x)
  (and (real? x)
       (positive? x)))

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


;; Hack only because can't analyze code yet :(

(define on/registry-table
  (make-table weak-keys: #t))

(define (on/registry-add! fn access cmp)
  (table-set! on/registry-table fn (cons access cmp))
  fn)

(define (on/registry? v)
  (and (procedure? v)
       (table-ref on/registry-table v #f)))

(define (on/registry-ref v)
  (table-ref on/registry-table v))

(define (on/registry-maybe-ref v)
  (table-ref on/registry-table v #f))

(define-macro* (on/registry access cmp)
  (let ((ACCESS (gensym 'access))
	(CMP (gensym 'cmp)))
    `(let ((,ACCESS ,access)
	   (,CMP ,cmp))
       (on/registry-add! (first-then 2 ,ACCESS ,CMP)
			 ,ACCESS ,CMP))))

;;/hack.

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


(define (box-inc! b)
  (let ((v (inc (unbox b))))
    (set-box! b v)
    v))
(define (box-dec! b)
  (let ((v (dec (unbox b))))
    (set-box! b v)
    v))

(TEST
 > (define b (box 10))
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

(define (box-update! b fn)
  (let ((v (fn (unbox b))))
    (set-box! b v)
    v))

(define (box-add! b x)
  (let ((v (+ (unbox b) x)))
    (set-box! b v)
    v))

;; (Just in the end it *does* get you. Scheme is inconsistent?)
(define box-set! set-box!) ;; ?

;; another crazy name?:
(define bitwise-or bitwise-ior)


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


(define (symbol-value-or sym thunk)
  (with-exception-catcher
   (lambda (e)
     (if (unbound-global-exception? e)
	 (thunk)
	 (raise e)))
   (lambda ()
     (eval sym))))


(define unbound:type-tag (vector 'unbound))

(define (make-unbound info)
  ;; info should probably be a symbol
  (vector unbound:type-tag info))

(define (unbound? v)
  (or (##unbound? v)
      (and (vector? v)
	   (eq? (vector-ref v 0) unbound:type-tag))))


;; This variant is using closures instead of eval, thus works on
;; lexicals, too. To make it work with e.g. define-module, use the
;; unbound "protocol" defined above by default.

(define (thunk-symbol-value-or sym-thunk or-thunk #!optional strict?)
  (with-exception-catcher
   (lambda (e) (if (unbound-global-exception? e) (or-thunk) (raise e)))
   (lambda ()
     (let ((v (sym-thunk)))
       (if (if strict?
	       (##unbound? v)
	       (unbound? v))
	   (or-thunk)
	   v)))))

(define-macro* (macro-symbol-value-or sym thunk)
  (if (symbol? (source-code sym))
      `(thunk-symbol-value-or
	(lambda () ,sym)
	,thunk
	#f)
      (source-error sym "not a symbol")))


(define-macro* (define-parameter name default-value)
  `(define ,name (make-parameter ,default-value)))


(define-parameter current-show-def show-procedure-location)

(define-macro* (show-def expr)
  (let ((expr* (source-code expr)))
    `((current-show-def)
      ,(if (symbol? expr*)
	   ;; XX A tiny bit evil: checks macro expander
	   ;; *first*. I.e. relies on current fact that if macro
	   ;; expander is defined then there's also a macro and it can
	   ;; never be unbound in the toplevel. Other way around would
	   ;; be more future safe?
	   (if (define-macro-star-maybe-ref expr*)
	       (symbol-append "expander#" (source-code expr))
	       expr)
	   expr))))


(define-macro* (define-if-not-defined name expr)
  (assert* symbol? name
	   (lambda (name)
	     `(define ,name
		(thunk-symbol-value-or (lambda () ,name)
				       (lambda () ,expr)
				       #f)))))

(define-macro* (define-if-not-defined-strict name expr)
  (assert* symbol? name
	   (lambda (name)
	     `(define ,name
		(thunk-symbol-value-or (lambda () ,name)
				       (lambda () ,expr)
				       #t)))))

(TEST
 > (define-if-not-defined abczxfwef 10)
 > abczxfwef
 10
 > (define-if-not-defined abczxfwef 11)
 > abczxfwef
 10

 > (define-if-not-defined cj-env-test:unbound (make-unbound 'define-module))
 > (define a cj-env-test:unbound)
 > (define-if-not-defined cj-env-test:unbound (make-unbound 'define-module))
 > (eq? a cj-env-test:unbound)
 #f

 > (define-if-not-defined-strict cj-env-test:unbound2 (make-unbound 'define-module))
 > (define a cj-env-test:unbound2)
 > (define-if-not-defined-strict cj-env-test:unbound2 (make-unbound 'define-module))
 > (eq? a cj-env-test:unbound2)
 #t
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

;; tests see cj-env-test


(define (keyword->symbol v)
  (string->symbol (keyword->string v)))

(define (symbol->keyword v)
  (string->keyword (symbol->string v)))


;; ~sigh, Gambit already defines |fixnum?|
(define bignum? ##bignum?)

