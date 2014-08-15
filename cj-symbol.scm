;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.cj-env))


(define symbol<?
  (on symbol->string string<?))

;; a gensym that returns interned symbols

(define gensym-prefix "GEN:")

(define gensym-count (make-parameter 0))
;; parameter not for thread safety, but for easy temporary override

(define (gensym-count-next)
  (let ((v (gensym-count)))
    (gensym-count (inc v))
    v))

(define (gensym #!optional maybe-name)
  (let* ((name (or maybe-name ""))
	 (cnt (number->string (gensym-count-next))))
    (symbol-append gensym-prefix name "-" cnt)))


(define (interned-symbol? v)
  (and (symbol? v)
       (not (uninterned-symbol? v))))

(define (cj-gensym? v)
  (and (interned-symbol? v)
       (let* ((s (symbol->string v))
	      (len (string-length s))
	      (plen (string-length gensym-prefix)))
	 (and (> len plen)
	      (string=? (substring s 0 plen)
			gensym-prefix)))))

(define (cj-gensym-maybe-name v)
  (if (cj-gensym? v)
      ;; XX what string libraries do we have available at this
      ;; point? assume that none?
      (let* ((str (symbol->string v))
	     (len (string-length str))
	     (startpos (string-length gensym-prefix))
	     (endpos (let lp ((i startpos))
		       (if (eq? (string-ref str i) #\-)
			   i
			   (lp (inc i))))))
	(if (= startpos endpos)
	    #f
	    (substring str startpos endpos)))
      (error "not a cj-gensym:" v)))

(TEST
 > (cj-gensym-maybe-name 'GEN:abc-3349)
 "abc"
 > (cj-gensym-maybe-name 'GEN:-3349)
 #f)


(define (any-gensym? v)
  (or (uninterned-symbol? v)
      (cj-gensym? v)))

(define (make-syntax-equal? TYPE?)
  (lambda (a b)
    (let ((equivalent?
	   ;; allow alpha conversion between a and b
	   (let ((a->b (make-table test: eq?))
		 (b->a (make-table test: eq?)))
	     (lambda (a b)
	       (let ((check
		      (lambda (a->b a b)
			(cond ((table-ref a->b a #f)
			       => (lambda (expected)
				    (eq? b expected)))
			      (else
			       ;; fresh name, accept
			       (table-set! a->b a b)
			       #t)))))
		 (and (check a->b a b)
		      (check b->a b a)))))))
      (let rec ((a a)
		(b b))
	;; check for TYPE *first*, because in that case we have to
	;; *forbid* comparison with equal?/eq?.
	(cond ((and (TYPE? a) (TYPE? b))
	       (equivalent? a b))
	      ((and (pair? a) (pair? b))
	       (and (rec (car a)
			 (car b))
		    (rec (cdr a)
			 (cdr b))))
	      ((and (vector? a) (vector? b))
	       (rec (vector->list a)
		    (vector->list b)))
	      ((and (box? a) (box? b))
	       (rec (unbox a)
		    (unbox b)))
	      (else
	       (equal? a b)))))))

(define syntax-equal? (make-syntax-equal? any-gensym?))
(define syntax-equal?* (make-syntax-equal? symbol?))

(TEST
 > (syntax-equal? 'a 'b)
 #f
 > (syntax-equal?* 'a 'b)
 #t
 > (syntax-equal? 'a 'a)
 #t
 > (syntax-equal?* 'a 'a)
 #t
 > (syntax-equal? '(a b c) '(a b "c"))
 #f
 > (syntax-equal?* '(a b c) '(a b "c"))
 #f
 > (syntax-equal? '(a b "c") '(a b "c"))
 #t
 > (syntax-equal?* '(a b "c") '(a b "c"))
 #t
 > (syntax-equal? '(a GEN:b "c") '(a GEN:b "c"))
 #t
 > (syntax-equal? '(a GEN:b "c") '(a GEN:x "c"))
 #t
 > (syntax-equal?* '(a GEN:b "c") '(a GEN:x "c"))
 #t
 > (syntax-equal? '(a GEN:b "c") '(a #:g23 "c"))
 #t
 > (syntax-equal? '(a GEN:b GEN:c "c") '(a #:g23 #:g23 "c"))
 #t
 > (syntax-equal? '(a GEN:b GEN:b "c") '(a #:g23 #:g23 "c"))
 #f
 > (syntax-equal? '(a GEN:b GEN:c "c") (let ((v (##gensym))) `(a ,v ,v "c")))
 #f
 > (syntax-equal? '(a GEN:b GEN:b "c") (let ((v (##gensym))) `(a ,v ,v "c")))
 #t

 > (syntax-equal? '(GEN:a GEN:b GEN:c) '(GEN:a GEN:c GEN:b))
 #t
 > (syntax-equal? '(GEN:a GEN:b GEN:b GEN:c) '(GEN:a GEN:c GEN:b GEN:b))
 #f
 
 > (syntax-equal?* '(a b c) '(a c b))
 #t
 > (syntax-equal?* '(a b b c) '(a f g g))
 #f
 > (syntax-equal?* '(a b b c) '(a c b b))
 #f
 )

;; related, thus put here, too:

(define-macro* (with-gensyms vars . body)
  (match-list*
   vars
   (vars
    `(let ,(map (lambda (v)
		  `(,v (gensym ',v)))
		vars)
       ,@body))))

(define-macro* (with-gensym var . body)
  `(with-gensyms (,var) ,@body))

(define-macro* (symbol-case expr . clauses)
  (with-gensyms
   (V V*)
   `(let ((,V ,expr))
      (assert*1 symbol? ,V
		(lambda (,V*)
		  (case ,V*
		    ;; XXX should check clauses for whether they really
		    ;; check for symbols...
		    ,@clauses))))))
