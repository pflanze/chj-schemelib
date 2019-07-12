;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 cj-struct
	 (cj-source-util-2 assert)
	 (scheme-meta self-quoting?)
	 (cj-gambit-sys procedure-name)
	 (srfi-11 values? values->list)
	 ;; dot-oo depends on cj-env already, and we want to add on/registry.show :
	 (cj-env on/registry? on/registry-ref natural0?)
	 debuggable-promise
	 ;; ^ now we have to always load it. But, have
	 ;; debuggable-promise-everywhere now which is optional.
	 (lazy-debug S)
	 cj-source
	 (string-util-2 string-starts-with? string-ends-with?)
	 (cj-path path-absolute?)
         (list-util-1 improper->proper-map)
         (srfi-1 cons*)
	 test)


(export (method .show)
	(method .show-string)
	try-show
        promise#
	procedure#
        improper-list?
        improper-list
	#!optional
	toplevel-procedure?
	struct-values)

(include "cj-standarddeclares.scm")


(define. (self-quoting.show v)
  v)

(define. (symbol.show v)
  `(quote ,v))


;; XX if not struct. ugly implicit assumption through ordering
;; currently. More generic must be earlier.

(define. (vector.show v)
  `(vector ,@(map .show (vector->list v))))

(define. (pair.show v)
  `(cons ,(.show (car v))
	 ,(.show (cdr v))))

(define. (list.show v)
  (cons 'list (map .show v)))

;; Move to predicates?
(define (improper-list? v)
  (and (pair? v)
       (pair? (cdr v))
       (not (list? (cddr v)))))

;; but then this ('the constructor') as well??
(define improper-list cons*)

(define. (improper-list.show v)
  (cons 'cons* (improper->proper-map .show v)))

(define. (values.show v)
  (cons 'values (map .show (values->list v))))

(define. (box.show v)
  `(box ,(.show (unbox v))))

;; good or bad idea?
(define. (void.show v)
  `(void))


;; XX should rename original ones instead? anyway, move to cj-source.scm
;; (define source make-source)
;; (define location make-location)
;; (define position make-position)
;; (define position* make-position*)
;; or actually use a different constructor than make-source, given its flat representation.

;;(define source:tag '[source1]) sigh, don't have it

(define (source* code
		 ;; location
		 location-container
		 location-line
		 location-column)
  (let ((c (if (string? location-container)
	       (if (path-absolute? location-container "/")
		   location-container
		   ;; do not use path-normalize here, makes a test
		   ;; like (equal? (eval (.show matchcases))
		   ;; matchcases) fail:
		   (string-append (current-directory) location-container))
	       location-container)))
    (make-source code (make-location c (make-position
					location-line
					location-column)))))

(define (@source-location-container s)
  (vector-ref s 2))
(define (@source-location-line&column s)
  (vector-ref s 3))
;; /move


(define (show:path-show-relative s)
  (let ((dir (current-directory)))
    (assert (string-ends-with? dir "/"))
    ;; and absolute, too:
    (assert (string-starts-with? dir "/"))
    (if (string-starts-with? s dir)
	(substring s (string-length dir) (string-length s))
	s)))

(define. (source.show v)
  (let ((lc (@source-location-line&column v)))
    `(source* ,(.show (source-code v))
	      ,(let ((c (@source-location-container v)))
		 (if (string? c)
		     (show:path-show-relative c)
		     (.show c)))
	      ,(position-line lc)
	      ,(position-column lc))))

;; XX add tests!!
;; > (equal? (eval (.show matchcases)) matchcases)
;; #t


(define (procedure# num)
  (let ((v (serial-number->object num)))
    (if (procedure? v)
	v
	(error "procedure#: not actually resolving to a procedure:"
	       num v))))

(define. (procedure.show p)
  `(procedure# ,(object->serial-number p)))

;; XX move? to predicates or rather cj-gambit-sys?
(define (toplevel-procedure? v)
  (and (procedure? v)
       (maybe-procedure-name v)
       #t))

(define. toplevel-procedure.show maybe-procedure-name)

;; structs:

;; XXX HACK for now, should generate for each struct according to its
;; (default) constructor:

(define. (struct.show v)
   ;; The HACK is: assumption that the constructor takes positional
   ;; arguments
  (cons (struct-constructor-name v)
	(map .show (struct-values v))))



(TEST
 > (.show '(1 2 3))
 (list 1 2 3)
 > (.show '(1 (2) . [3]))
 (cons* 1 (list 2) (vector 3))
 > (.show '(1 . [2]))
 (cons 1 (vector 2))
 > (.show (values (+ 1 2) (vector 2)))
 (values 3 (vector 2))
 > (improper-list? 'a)
 #f
 > (improper-list? '(a))
 #f
 > (improper-list? '(a . b))
 #f ;; since I want pair.show to happen here.
 > (improper-list? '(a c . b))
 #t)



(define (try-show v)
  (with-exception-catcher
   (lambda (e)
     `(try-show ,v))
   (lambda ()
     (.show v))))



;; (define. (u8vector.show v)
;;   `(u8vector ,@(u8vector->list v)))
;; now defined in u8vector0.scm

(define. (s8vector.show v)
  `(s8vector ,@(s8vector->list v)))

(define. (on/registry.show v)
  (let* ((p (on/registry-ref v))
	 (access (car p))
	 (cmp (cdr p)))
    `(on/registry ,(.show access)
		  ,(.show cmp))))



(define (promise# n)
  (let ((v (serial-number->object n)))
    (if (promise? v)
	v
	(error "not a promise:" v))))

(define. (debuggable-promise.show v)
  (if (debuggable-promise? v)
      (if (@debuggable-promise-evaluated? v)
	  ;; pre-force all the way before calling .show again (so that
	  ;; things like list? will match, although this may trigger
	  ;; n^2 complexity issues more easily? XX)
	  (.show (S v))
	  `(promise# ,(object->serial-number v)))
      ;; shouldn't happen 'usually' (ever this question, unsafe direct call)
      (error "not a debuggable-promise:" v)))

(define. (##promise.show v)
  (if (##promise? v)
      (if (@promise-evaluated? v)
	  (.show (S v))
	  `(promise# ,(object->serial-number v)))
      ;; shouldn't happen 'usually' (ever this question, unsafe direct call)
      (error "not a ##promise:" v)))


(define. (any.show-string v)
  (object->string (.show v)))


(TEST
 > (def (t n)
        (delay (cons n (t (inc n)))))
 > (def ns (t 10))
 > (car (force ns))
 10
 > (length (.show ns))
 3 ;; (cons 10 (promise# 46))  or similar
 > (equal? (eval (.show ns))
           ;; need to force here since .show strips the promise:
           (force ns))
 #t
 > (improper-list 1 2)
 (1 . 2)
 > (improper-list 1 2 3)
 (1 2 . 3)
 > (improper-list 1 2 3 4)
 (1 2 3 . 4))

