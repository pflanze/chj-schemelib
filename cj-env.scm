;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.cj-phasing)
	 (lib.cj-inline)
	 (lib.test))

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


(define-macro* (assert expr)
  `(if (not ,expr)
       (error ,(string-append "assertment failure: "
			      (scm:object->string (cj-desourcify expr))))))


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


(define-macro* (inc! v)
  (let ((V (gensym)))
    `(let ((,V (inc ,v)))
       (set! ,v ,V)
       ,V)))


;; treating null as false:

(define-inline (not* v)
  (or (eq? v #f)
      (eq? v '())))

(define-macro* (if* test yes #!optional no)
  `(if (##not (not* ,test))
       ,yes
       ,@(if (source-code no)
	     (list no)
	     '())))

(TEST
 > (if* #t 1 2)
 1
 > (if* #f 1 2)
 2
 > (if* '() 1 2)
 2
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


;; (copy from chjmodule's cj-expr.scm)
(define (read-all-expr #!optional (port (current-input-port)))
  ;; NOTE: does NOT return an expr. It returns a *list* of expr's.
  (let recur ()
    (let ((expr (##read-expr-from-port port)))
      (if (eof-object? expr) '()
	  (cons expr (recur))))))


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

(define-macro* (define-if-not-defined name expr)
  (assert* symbol? name
	   (lambda (name)
	     (let ((calc-name (symbol-append '_calc- name)))
	       `(begin
		  (define (,calc-name) ,expr)
		  (define ,name
		    (symbol-value-or ',name ,calc-name)))))))

(TEST
 > (define-if-not-defined abczxfwef 10)
 > abczxfwef
 10
 > (define-if-not-defined abczxfwef 11)
 > abczxfwef
 10
 )

