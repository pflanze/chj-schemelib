;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match-1
	 cj-phasing
	 optim-values
	 test
	 ;; (cj-env-1 inc dec) cj-source, sigh
	 (fixnum inc dec)
	 (named named-lambda)
	 list-util ;; let-pair
	 (list-util-1 improper-map)
	 ;; (cj-functional compose-function) now avoided
	 )

(export (macro let*-values)
	improper-mapfold ;; move??
	values->vector
	values->list
	list->values
	(macro letv)
	(macro with-values)
	(macro lambda-values)
	(macro lambda-values/force)
	(macro apply-values)
	applying-values
	(macro define-values)
	values-ref
	fst
	snd
	3rd
	4th
	5th
	(macro @fst)
	(macro @snd)
	(macro @3rd)
	(macro @4th)
	(macro @5th)
	values?
	values-equal?
	values-map)

(include "cj-standarddeclares.scm")


(define-macro* (let*-values bindforms* . body)
  (match-list*
   bindforms*
   (bindforms
    (fold-right (lambda (bindform body)
		  (match-list*
		   bindform
		   ((vars expr)
		    (sourcify
		     `(%call-with-values (lambda ()
					  ,expr)
			(lambda ,vars
			  ,body))
		     vars))))
		`(begin ,@body)
		bindforms))))

(TEST
 > (expansion#let*-values (((a b) (values 1 2)) ((c d) (values (inc b) (inc a)))) c)
 (%call-with-values
  (lambda () (values 1 2))
  (lambda (a b)
    (%call-with-values
     (lambda () (values (inc b) (inc a)))
     (lambda (c d) (begin c)))))
 > (let*-values (((a b) (values 1 2)) ((c d) (values (inc b) (inc a)))) c)
 3
 )

;; Doing kind of map and fold at the same time.
;; (fn state v) -> (values state* v*)
(define (improper-mapfold fn state l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
	   (values state
		   tail))
	  ((pair? l)
	   (let*-values
	    (((state* l*) (rec (cdr l)))
	     ((state** v*) (fn state* (car l))))
	    (values state**
		    (cons v* l*))))
	  (else
	   (fn state l)))))

(define (values->vector v)
  (if (values? v)
      (##vector-copy v)
      ;; Okay?:
      (vector v)))

(define (values->list v)
  (vector->list (values->vector v)))

(define (list->values l)
  (apply values l))

(TEST
 > (values->vector (improper-mapfold (lambda (a b) (values (dec a) (inc b))) '100 '(1 2 . 3)))
 #(97 (2 3 . 4))
 ;; > ((compose-function values->vector list->values) '(a b c))
 ;; (a b c)
 ;;OHH compose-function applies values. nogo.XX
 > (values->vector (list->values '(a b c)))
 #(a b c)
 )


(define-macro* (let-values bindforms* . body)
  (match-list*
   bindforms*
   (bindforms
    (let-pair
     ((varmap body)
      (fold-right
       (lambda (bindform varmap.body)
	 (with-pair
	  varmap.body

	  (match-list*
	   bindform
	   ((vars expr)
	    (let*-values
		(((varmap VARS) (improper-mapfold
				 (lambda (varmap v)
				   (let ((V (assert* symbol? v
						     gensym)))
				     (values (cons (cons v V)
						   varmap)
					     V)))
				 varmap 
				 (source-code vars))))
	      (cons varmap
		    (lambda (varmap)
		      (sourcify
		       `(%call-with-values (lambda ()
					     ,expr)
					   (lambda ,VARS
					     ,(body varmap)))
		       vars))))))))
       (cons '()
	     (lambda (varmap)
	       `((lambda ,(map car varmap)
		   ,@body)
		 ,@(map cdr varmap))))
       bindforms))

     (body varmap)))))


(TEST
 > (require (cj-symbol)))
(TEST
 > (let-values (((a . b) (values 1 2))) a)
 1
 > (let-values (((a . b) (values 1 2))) b)
 (2)
 ;; > (let ((one 1)) (let-values (((one . rest) (values 100 200)) ((two . rest) (values 2))) (vector one two rest)))
 ;; *** ERROR IN (console)@31.69 -- Duplicate parameter in parameter list
 ;; 1> 
 ;; heh
 > (let ((one 1)) (let-values (((one . rest) (values 100 200)) ((two . rest2) (values (inc one)))) (vector one two rest rest2)))
 #(100 2 (200) ())

 > (define TEST:equal? syntax-equal?)
 > (expansion#let-values (((one . rest) (values 100 200)) ((two . rest2) (values (inc one)))) (vector one two rest rest2))
 (%call-with-values
  (lambda () (values 100 200))
  (lambda (GEN:one4532 . GEN:rest4531)
    (%call-with-values
     (lambda () (values (inc one)))
     (lambda (GEN:two4530 . GEN:rest24529)
       ((lambda (one rest two rest2) (vector one two rest rest2))
	GEN:one4532
	GEN:rest4531
	GEN:two4530
	GEN:rest24529)))))

 )

;; Not part of srfi-11, just bundling here:

;; |bind| as suggested in
;; http://srfi.schemers.org/srfi-11/mail-archive/msg00008.html but
;; using another name to not prevent potential future usage of the
;; bind name for something else

(define-macro* (letv formal . body)
  `(let-values (,formal) ,@body))


;; XXX: This relies on values being passable through variables (tuple
;; semantic, like in ML/Haskell, or like a Scheme vector); Gambit
;; works this way, but this behaviour is not standardized, and Common
;; Lisp differs (it drops all but the first value when passing
;; multiple values to a single variable) and some people find the
;; latter behaviour useful, so Scheme might standardize this way some
;; time. In that case, this code and code using with-values will have
;; to be changed (to e.g. use vectors or lists instead of values):

;; mostly copy-paste from with-pair
(define-macro* (with-values var* . body)
  (assert* symbol? var*
	   (lambda (var)
	     (let ((l (string-split (symbol->string var) #\.)))
	       `(let-values ((,(map string->symbol l) ,var*))
		  ,@body)))))

(TEST
 > (let ((a.b (values 10 20))) (with-values a.b (cons a b)))
 (10 . 20)
 )

;;;
;;;; lambda-values
;;;
;;; Syntax to interpret sub-lists of identifiers as being applied
;;; multiple-values
;;;

(both-times
 (define (lambda-values-expand force?)
   (named-lambda lp
     (*in-ids out-ids body) ;; body is required to be 1 form
     (let ((in-ids (source-code *in-ids)))
       (cond ((null? in-ids)
	      `(lambda ,(reverse out-ids)
		 ,body))
	     ((pair? in-ids)
	      (let-pair ((*id-or-ids r) (source-code in-ids))
			(if (pair? (source-code *id-or-ids))
			    (let ((id* (gensym)))
			      (lp r
				  (cons id* out-ids)
				  `(%call-with-values
				    (lambda ()
				      ,(if force?
					   ;; XXX optim.: avoid force overhead?
					   `(force ,id*)
					   id*))
				    ;; using the 'cheap' way of recursion:
				    (lambda-values ,*id-or-ids
						   ,body))))
			    (lp r
				(cons *id-or-ids out-ids)
				body))))
	     ((symbol? in-ids)
	      ;; rest argument
	      `(lambda ,(append (reverse out-ids) *in-ids)
		 ,body))
	     (else
	      (raise-source-error *in-ids "invalid type for identifier")))))))

(TEST
 > (define TEST:equal? syntax-equal?)
 > ((lambda-values-expand #f) '(a b c) '() 'mybody)
 (lambda (a b c) mybody)
 > ((lambda-values-expand #f) '(a (b1 b2) c) '() 'mybody)
 (lambda (a GEN:201 c)
   (%call-with-values (lambda () GEN:201) (lambda-values (b1 b2) mybody)))
 > ((lambda-values-expand #f) '(a (b1 . b2) c) '() 'mybody)
 (lambda (a GEN:202 c)
   (%call-with-values (lambda () GEN:202) (lambda-values (b1 . b2) mybody)))
 > ((lambda-values-expand #f) '(a (b1 . b2) . c) '() 'mybody)
 (lambda (a GEN:71 . c)
   (%call-with-values (lambda () GEN:71) (lambda-values (b1 . b2) mybody)))
 )

(define-macro* (lambda-values ids . body)
  ((lambda-values-expand #f) ids
   '()
   (cons 'begin body)))

(define-macro* (lambda-values/force ids . body)
  ((lambda-values-expand #t) ids
   '()
   (cons 'begin body)))

(TEST
 > ((lambda-values (x) (* x x)) 10)
 100
 > ((lambda-values ((x y)) (* x y)) (values 10 11))
 110
 > ((lambda-values ((x y) z . r) (cons (* x y z) r)) (values 10 11) 2 'a 'b)
 (220 a b)
 > ((lambda-values ((x (y z)) . r) (cons (* x y z) r)) (values 10 (values 11 3)) 'a 'b)
 (330 a b)
 )


;; Putting here just because being values related, too:

;; Analogon to |apply| but for values. Unlike |apply|, this can't be a
;; function if it is to be used in the same syntactical form.

(define-macro* (apply-values f-expr arg-expr)
  `(%call-with-values (lambda ()
			,arg-expr)
		      ,f-expr))

(TEST
 > (apply-values / (values 1 3))
 1/3
 )


(define (applying-values f)
  (lambda (vals)
    (if (values? vals)
	(case (@values-length vals)
	  ((0) (f))
	  ((1) (f (@values-ref vals 0)))
	  ((2) (f (@values-ref vals 0)
		  (@values-ref vals 1)))
	  ((3) (f (@values-ref vals 0)
		  (@values-ref vals 1)
		  (@values-ref vals 2)))
	  ((4) (f (@values-ref vals 0)
		  (@values-ref vals 1)
		  (@values-ref vals 2)
		  (@values-ref vals 3)))
	  (else
	   (apply f (values->list vals))))
	(f vals))))

(TEST
 > (map (applying-values vector)
	(list (values)
	      (values 10)
	      (values 11 12)
	      (values 11 12 13 14 15 16)))
 ([]
  [10]
  [11 12]
  [11 12 13 14 15 16]))


(define-macro* (define-values vars* expr)
  (define (flatten1 lis)
    (improper-fold (lambda (v l)
		     (cons v l))
		   '()
		   lis))
  (let* ((varsflat (map (lambda (v)
			  (assert* symbol? v
				   values))
			(flatten1 (source-code vars*))))
	 (varstmptop (map gensym varsflat))
	 (varslex (improper-map
		   ;; (compose-function gensym source-code)
		   ;; avoid dependency cycle
		   (lambda (v)
		     (gensym (source-code v)))
		   (source-code vars*))))
    
    `(begin
       ,@(map (lambda (t)
		`(define ,t #f))
	      varstmptop)
       (let-values ((,varslex ,expr))
	 ,@(map (lambda (v* vgen)
		  `(set! ,v* ,vgen))
		varstmptop
		(flatten1 varslex)))
       ,@(map (lambda (v t)
		`(begin
		   (define ,v ,t)
		   (set! ,t #f)))
	      varsflat
	      varstmptop))))

(TEST
 > (define-values (a b . c) (values 1 2 3 4 5))
 > a
 1
 > b
 2
 > c
 (3 4 5)
 > (define TEST:equal? syntax-equal?)
 > (expansion#define-values (a b . c) (values 1 2 3 4 5))
(begin
  (define GEN:c449 #f)
  (define GEN:b450 #f)
  (define GEN:a451 #f)
  (let-values
   (((GEN:a452 GEN:b453 . GEN:c454) (values 1 2 3 4 5)))
   (set! GEN:c449 GEN:c454)
   (set! GEN:b450 GEN:b453)
   (set! GEN:a451 GEN:a452))
  (begin (define c GEN:c449) (set! GEN:c449 #f))
  (begin (define b GEN:b450) (set! GEN:b450 #f))
  (begin (define a GEN:a451) (set! GEN:a451 #f)))
 )

(define (values-ref v i)
  (define (err len i)
    (error "values-ref: index out of range (len,i):" len i))
  (if (values? v)
      (let ((len (@values-length v)))
	(if (and (natural0? i)
		 (< i len))
	    (@values-ref v i)
	    (err len i)))
      (if (= i 0)
	  v
	  (err 1 i))))

;; (define fst (cut values-ref <> 0))
;; (define snd (cut values-ref <> 1))
;; (define 3rd (cut values-ref <> 2))
;;dependency issue?
(define fst (lambda (<>) (values-ref <> 0)))
(define snd (lambda (<>) (values-ref <> 1)))
(define 3rd (lambda (<>) (values-ref <> 2)))
(define 4th (lambda (<>) (values-ref <> 3)))
(define 5th (lambda (<>) (values-ref <> 4)))

(define-macro* (@fst v) `(@values-ref ,v 0))
(define-macro* (@snd v) `(@values-ref ,v 1))
(define-macro* (@3rd v) `(@values-ref ,v 2))
(define-macro* (@4th v) `(@values-ref ,v 3))
(define-macro* (@5th v) `(@values-ref ,v 4))

(TEST
 > (require (test-random)))

(TEST
 > (fst (values 'a))
 a
 > (fst 'A)
 A
 > (fst (values 'a 'b))
 a
 > (snd (values 'a 'b))
 b
 > (snd (values 'a 'b 'c))
 b
 > (3rd (values 'a 'b 'c))
 c
 > (%try-error (3rd (values 'a 'b)))
 #(error "values-ref: index out of range (len,i):" 2 2)
 )

;; This is a pretty broken-Gambit workaround:

(define (values? v) ;; only a tuple (lowlevel understanding (as in
		    ;; current ->type))
  (##values? v))

(define (values-equal? a b)
  (cond ((and (values? a)
	      (values? b))
	 ;; should recurse here!
	 (equal? (##vector->list a)
		 (##vector->list b)))
	(else
	 ;; should recurse here!
	 (equal? a b))))

(TEST
 > (values-equal? (values 1 2) (values 1 2))
 #t
 > (values-equal? (values 1 2) (values 1 3))
 #f
 > (values-equal? (values 1 2) 3)
 #f
 )

(define (values-map fn v)
  ;; there's no ##make-values, so:
  (if (values? v)
      (case (@values-length v)
        ((0) (values))
        ((2) (values (fn (@fst v))
                     (fn (@snd v))))
        ((3) (values (fn (@fst v))
                     (fn (@snd v))
                     (fn (@3rd v))))
        (else
         (apply values (map fn (values->list v)))))
      (fn v)))

(TEST
 > (define (t . args)
     (values->vector (values-map (lambda (x) (* x x))
                                 (apply values args))))
 > (t)
 []
 > (t 2)
 [4]
 > (t 2 3 4)
 [4 9 16]
 > (t 2 3 4 5)
 [4 9 16 25])

