;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


;lib
(define-macro* (symbol-case* expr . clauses)
  (with-gensyms
   (V V*)
   `(let ((,V ,expr))
      (assert* symbol? ,V
	       (lambda (,V*)
		 (case ,V*
		   ;; XXX should check clauses for whether they really
		   ;; check for symbols...
		   ,@clauses))))))
;;/lib


;; destructuring bind

;; (list 1 2 x ,x) matches `(1 2 ,x ,X) with anything in X, binding x
;; to what X is

;; can handle source code


;; clause 'data type':
(define clause:parse
  (lambda (clause)
    (match* clause
	    ((test . body)
	     (values test body)))))

(define clause:test (compose fst clause:parse))
(define clause:body (compose snd clause:parse))

;; (define clause:testhead-xsym
;;   (lambda (clause)
;;     (assert* pair? (clause:test clause)
;; 	     (lambda (test)
;; 	       (assert* symbol? (car test)
;; 			identity)))))

(define clause:test-parse
  ;; (values constructor apply? rest)
  (lambda (clause)
    (assert* pair? (clause:test clause)
	     (lambda (test)
	       (assert* symbol? (car test)
			(lambda (hd)
			  (case hd
			    ((apply)
			     (assert* pair? (cdr test)
				      (lambda (test)
					(assert* symbol? (car test)
						 (lambda_
						  (values _
							  #t
							  (cdr test)))))))
			    (else
			     (values hd
				     #f
				     (cdr test))))))))))

(define clause:constructor-xsym (compose fst clause:test-parse))
(define clause:apply? (compose snd clause:test-parse))
(define clause:args (compose 3rd clause:test-parse))

(define clause:test-nargs
  ;; negative like improper-length if it's an n-ary application
  (lambda (clause)
    (let ((len (improper-length
		;; ^ just to handle error myself
		(source-code (clause:args clause)))))
      (if (negative? len)
	  (source-error clause "invalid function application (improper list)")
	  (if (clause:apply? clause)
	      (if (zero? len)
		  ;; is this just a stupid Scheme limitation?
		  (source-error (clause:test clause)
				"wrong number of arguments passed to apply")
		  (- len))
	      len)))))

(TEST
 > (clause:test-nargs '((list a b c) foo))
 3
 > (%try-syntax-error (clause:test-nargs '((list a b . c) foo)))
 #(source-error "invalid function application (improper list)")
 > (clause:test-nargs '((apply list a b c) foo))
 -3
 > (clause:test-nargs '((list) foo))
 0
 > (%try-syntax-error (clause:test-nargs '((apply list) foo)))
 #(source-error "wrong number of arguments passed to apply")
 )
;; / clause 

;; for now, only recognize unquotes in the top level of clause-test

(compile-time
 (define (extract-variables args*) ;; (values applicable-clause pos+var-s)
   (let rec ((args* args*)
	     (pos 0))
     (let ((args (source-code args*)))
       (if (null? args)
	   (values '()
		   '())
	   (let-pair
	    ((a* r*) args)
	    (letv ((appclr vars) (rec r* (inc pos)))
		  (let ((a (source-code a*)))
		    (if (and (pair? a)
			     (eq? 'unquote (source-code (car a)))
			     (pair? (source-code (cdr a)))
			     (null? (source-code (cdr (source-code (cdr a))))))
			(let ((var (car (source-code (cdr a)))))
			  (values (cons var appclr)
				  (cons (cons pos var) vars)))
			(values (cons a* appclr)
				vars))))))))))

(TEST
 > (values->vector (extract-variables '(a b c)))
 #((a b c)
   ())
 > (values->vector (extract-variables '(a ,b ,c)))
 #((a b c)
   ((1 . b) (2 . c)))
 )

(define clause->check
  (lambda (clause V remainder)
    (let*-values (((constructor apply? rest) (clause:test-parse clause))
		  ((clause-code-rest pos+vars) (extract-variables rest)))
      (let* ((just-list-ref
	      (lambda-pair ((pos var))
			   `(,var (list-ref ,V ,pos))))
	     (pos+var->code
	      ;; (XX: potential for optimization of list extraction)
	      (let ((nargs (clause:test-nargs clause)))
		(if (negative? nargs)
		    (let ((lastpos (dec (- nargs))))
		      (lambda-pair ((pos var))
				   (if (= pos lastpos)
				       `(,var (drop ,V ,pos))
				       (just-list-ref (cons pos var)))))
		    just-list-ref))))
	`(let ,(map pos+var->code
		    pos+vars)
	   ;; (of course, potential for optimizing accesses
	   ;; (constructions) here too, especially the pos'ed
	   ;; fields)
	   (if (equal? ,(cj-possibly-sourcify-deep
			 `(,@(if apply? '(apply) '())
			   ,constructor
			   ,@clause-code-rest)
			 (clause:test clause)) ,V)
	       (begin
		 ,@(clause:body clause))
	       ,remainder))))))

(TEST
 > (clause->check '((list a ,b c) (run b)) 'MYV 'REM)
 (let ((b (list-ref MYV 1))) (if (equal? (list a b c) MYV) (begin (run b)) REM))
 )

(define clauses->check
  (lambda (group V rem)
    (fold-right
     (lambda (clause rem)
       (clause->check clause
		      V
		      rem))
     rem
     group)))

(define (arity-compatible? inpa clausea)
  (if (negative? inpa)
      (error "bug")
      (if (negative? clausea)
	  (>= inpa (- -1 clausea))
	  (= inpa clausea))))

(TEST
 > (arity-compatible? 1 1)
 #t
 > (arity-compatible? 2 1)
 #f
 > (arity-compatible? 1 2)
 #f
 > (arity-compatible? 1 -1)
 #t
 > (arity-compatible? 2 -1)
 #t
 > (arity-compatible? 2 -3)
 #t
 > (arity-compatible? 1 -3)
 #f
 > (arity-compatible? 3 -3)
 #t
 )

(define handle-op-group
  (lambda (input opgroup)
    ;; XX assumes that there is only one group; should move V* V LEN up
    (let ((constructor (clause:constructor-xsym (source-code (car opgroup)))))
      (symbol-case*
       constructor
       ((list)
	;; XX later: reuse the same code for 'apply list'
	(let* ((grouped-by-nargs
		(list-group-by opgroup (on clause:test-nargs <))))
	  ;; split into n-ary and fixed arity cases:
	  (letv ((groups-nary groups-fixed)
		 (partition (compose* negative?
				      clause:test-nargs
				      car)
			    grouped-by-nargs))
		;; nary groups need to be given names so as to be reusable:
		(let* ((narity->name+group
			(map (lambda (group)
			       ;; first item in list is alist key 
			       (list (clause:test-nargs (car group))
				     (gensym)
				     group))
			     groups-nary))
		       (narity->name
			;; ((table accessor converters how?))
			(lambda (a)
			  (car (number-alist-ref narity->name+group a)))))
		  (with-gensyms
		   (V* V LEN NO-MATCH)
		   `(let* ((,V* ,input)
			   (,V (source-code ,V*))
			   (,LEN (improper-length ,V))
			   (,NO-MATCH (lambda ()
					(source-error ,V* "no match"))))
		      (let ,(map (lambda_
				  (apply
				   (lambda (nargs name group)
				     (with-gensyms
				      (CONT)
				      `(,name
					(lambda (,CONT)
					  ,(clauses->check group
							   V
							   `(,CONT))))))
				   _))
				 narity->name+group)
			(if (negative? ,LEN)
			    (source-error
			     ,V*
			     "matching of improper lists is not implemented")
			    (case ,LEN
			      ,@(map
				 (lambda (group)
				   (let ((nargs
					  (clause:test-nargs (car group))))
				     `((,nargs)
				       ,(fold-right
					 (lambda (clause rem)
					   (let ((cnargs
						  (clause:test-nargs clause)))
					     (if (arity-compatible? nargs
								    cnargs)
						 (if (negative? cnargs)
						     `(,(narity->name cnargs)
						       (lambda ()
							 ,rem))
						     (clause->check clause
								    V
								    rem))
						 rem)))
					 `(,NO-MATCH)
					 opgroup))))
				 groups-fixed)
			      (else
			       ;; possible nary clauses; hm also still
			       ;; keep the clause ordering, so have to
			       ;; re-check arity for each and every
			       ;; one of them.
			       ,(fold-right
				 (lambda (clause rem)
				   (let ((nargs (clause:test-nargs clause)))
				     (with-gensym
				      REM
				      `(let ((,REM (lambda ()
						     ,rem)))
					 (if (>= ,LEN ,(- -1 nargs))
					     (,(narity->name nargs)
					      ,REM)
					     (,REM))))))
				 `(source-error ,V* "no match")
				 (filter clause:apply?
					 opgroup))))))))))))
       (else
	(source-error constructor
		      "only list matching is implemented"))))))

(define-macro*d (match input . clauses)
  ;; group according to type of datum
  (let* ((clausegroups
	  (list-group-by clauses
			 (on clause:constructor-xsym
			     symbol<?))))
    (case (length clausegroups)
      ((1)
       (handle-op-group input (car clausegroups)))
      ((0)
       (source-error stx "missing clauses"))
      (else
       (source-error stx "match currently only implements list matching")))))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (match '(1 2 3) ((list 1 2 3) 'found))
 found
 > (%try-syntax-error (match '(1 2) ((list 1 2 3) 'found)))
 #(source-error "no match")
 > (match '(1 2) ((list 1 2 3) 'found) ((list 1 2) 'found2))
 found2
 > (match '(1 2 3) ((list 1 2 3) 'found) ((list 1 2 4) 'found2))
 found
 > (match '(1 2 4) ((list 1 2 3) 'found) ((list 1 2 4) 'found2))
 found2
 > (match '(1 2 4) ((list 1 2) 'found) ((list 1 2 4) 'found2))
 found2
 > (match '(1 2) ((list 1 2) 'found) ((list 1 2 4) 'found2))
 found
 > (define (t v) (match v
			((list 1 2) 'found12)
			((list 1 ,x) (list 'foundx x))
			((list 1 ,x 4) (list 'foundx4 x))))
 > (t '(1 2))
 found12
 > (t '(1 3))
 (foundx 3)
 > (t '(1 3 4))
 (foundx4 3)
 > (%try-syntax-error (t '(1 3 3)))
 #(source-error "no match")
 > (define (t v) (match v
			((list 1 2) 'found12)
			((list 1 ,x) (list 'foundx x))
			((list 1 ,x 4) (list 'foundx4 x))
			((apply list x ,y ,r) (list 'foundr x y r))))
 > (t '(1 2))
 found12
 > (t '(1 2 4))
 (foundx4 2)
 > (t '(1 2 3))
 (foundr 1 2 (3))
 > (t '(1 2 3 4))
 (foundr 1 2 (3 4))
 > (t '(1 2 4 3))
 (foundr 1 2 (4 3))
 )
