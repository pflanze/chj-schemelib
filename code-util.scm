;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-symbol with-gensym with-gensyms)
	 (list-util-1 map/iota)
         (list-util lambda-pair)
         ;; cj-typed -- can't use
	 test
	 (cj-symbol syntax-equal?))

(export early-bind-expressions)


;; Avoid macro's late binding: evaluate used expressions only
;; once[1]. But only if they are not just symbols: leave symbol
;; references late-bound. This can allow to resolve mutual (really:
;; recursive) definitions without an extra function wrapper, and can
;; avoid the need for allocating closures.

;; [1] *But*, now (starting in 2020) bind those lazily. So, those
;; expressions are only evaluated once, but still late. Thus
;; recursive definitions will always work.

(define (early-bind-expressions:expr+ expr)
  "For `expr`, decide whether to evaluate it outside the lambda.

   Returns: (pair-of (maybe symbol?) expr?)"
  (cons
   ;; what variable name the end code should use to hold the result of
   ;; the evaluation of expr, if any:
   (if (symbol? (source-code expr))
       #f
       (gensym))
   ;; the expression the end code should use (which is just the
   ;; original expression of course):
   expr))

(define (early-bind-expressions:expr+s . exprs)
  (early-bind-expressions:expr+s* exprs))

(define (early-bind-expressions:use-expr+ expr+)
  ;; if a symbol was created, use it, otherwise the original
  ;; var-expr name
  (cond ((car expr+)
         => (lambda (var)
              `(force ,var)))
        (else
         (cdr expr+))))

(define (early-bind-expressions:expr+s-ref-expr* expr+s i)
  "From the list of var/expr pairings, pick the `i`th and from that
give the expression that gives the function or macro to be
called (which may be a `(force )` expression."
  (early-bind-expressions:use-expr+ (list-ref expr+s i)))

(define (early-bind-expressions:wrap expr+s code)
  ;; Build code to evaluate the expressions used by the end code that
  ;; need evaluation. This is simply those expr+ which have a
  ;; generated symbol name.
  (let ((need-eval (filter car expr+s)))
    (if (null? need-eval)
	code
	`(##let ,(map (lambda-pair ((var expr))
                              `(,var (delay ,expr)))
                      need-eval)
                ,code))))


;; For "rest argument" support (a single variable in the macro
;; expander holding all expressions to be interpolated):

(define (early-bind-expressions:expr+s* exprs)
  (map early-bind-expressions:expr+ exprs))

(define (early-bind-expressions:bind-for-use expr+s)
  (map early-bind-expressions:use-expr+ expr+s))


;; codegen-expr is generating code that is being wrapped to early-bind
;; the variables (which are supposedly used in codegen-expr) that are
;; listed in var-of-exprs:

(define-macro* (early-bind-expressions var-of-exprS codegen-expr)
  (with-gensyms
   (EXPR+S)
   (if (pair? (source-code var-of-exprS))

       ;; (i.e. {var-of-expr}s)
       (let ((var-of-exprs (source-code var-of-exprS)))
	 `(let ((,EXPR+S (early-bind-expressions:expr+s ,@var-of-exprs)))
	    (early-bind-expressions:wrap
	     ,EXPR+S
	     ;; re-bind var-of-exprs in the macro expander to the gensyms
	     ;; or their original, depending on whether they are bound to
	     ;; an expression or a symbol:
	     (let ,(map/iota
		    (lambda (var-of-expr i)
		      `(,var-of-expr
			(early-bind-expressions:expr+s-ref-expr* ,EXPR+S ,i)))
		    var-of-exprs)
	       ,codegen-expr))))
      
       ;; Only 1 variable given, assumed to hold a list of
       ;; var-of-exprs (i.e. var-of-{exprs})
       `(let ((,EXPR+S (early-bind-expressions:expr+s* ,var-of-exprS)))
	  (early-bind-expressions:wrap
	   ,EXPR+S
	   (let ((,var-of-exprS (early-bind-expressions:bind-for-use ,EXPR+S)))
	     ,codegen-expr))))))

(TEST
 > (define TEST:equal? syntax-equal?)

 > (expansion#early-bind-expressions
    (t1? t2?)
    `(lambda (v)
       (and (pair? v)
	    (,t1? (car v))
	    (,t2? (cdr v)))))
 (let ((GEN:EXPR+S-2454 (early-bind-expressions:expr+s t1? t2?)))
   (early-bind-expressions:wrap
    GEN:EXPR+S-2454
    (let ((t1? (early-bind-expressions:expr+s-ref-expr* GEN:EXPR+S-2454 0))
	  (t2? (early-bind-expressions:expr+s-ref-expr* GEN:EXPR+S-2454 1)))
      `(lambda (v) (and (pair? v) (,t1? (car v)) (,t2? (cdr v)))))))

 > (let ((t1? 'number?) (t2? 'string?))
     (let ((GEN:EXPR+S-2454 (early-bind-expressions:expr+s t1? t2?)))
       (early-bind-expressions:wrap
	GEN:EXPR+S-2454
	(let ((t1? (early-bind-expressions:expr+s-ref-expr* GEN:EXPR+S-2454 0))
	      (t2? (early-bind-expressions:expr+s-ref-expr* GEN:EXPR+S-2454 1)))
	  `(lambda (v) (and (pair? v) (,t1? (car v)) (,t2? (cdr v))))))))
 (lambda (v) (and (pair? v) (number? (car v)) (string? (cdr v))))

 > (eval `(let ((t1? 'number?) (t2? 'string?))
	    ,(expansion#early-bind-expressions
	      (t1? t2?)
	      `(lambda (v)
		 (and (pair? v)
		      (,t1? (car v))
		      (,t2? (cdr v)))))))
 (lambda (v) (and (pair? v) (number? (car v)) (string? (cdr v))))

 > (let ((t1? 'number?) (t2? 'string?))
     (early-bind-expressions
      (t1? t2?)
      `(lambda (v)
	 (and (pair? v)
	      (,t1? (car v))
	      (,t2? (cdr v))))))
 (lambda (v) (and (pair? v) (number? (car v)) (string? (cdr v))))
 
 ;; ^^  kill that sh

 > (define-macro* (my-pair-of t1? t2?)
     (early-bind-expressions
      (t1? t2?)
      `(lambda (v)
 	 (and (pair? v)
 	      (,t1? (car v))
 	      (,t2? (cdr v))))))

 > (expansion#my-pair-of a? b?)
 (lambda (v) (and (pair? v)
 	     (a? (car v))
 	     (b? (cdr v))))
 > (expansion#my-pair-of (maybe a?) b?)
 (##let ((GEN:-2449 (delay (maybe a?))))
 	(lambda (v)
 	  (and (pair? v)
 	       ((force GEN:-2449) (car v))
 	       (b? (cdr v)))))
 )


