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

(compile-time
 (define (extract-variables clause*) ;; (values applicable-clause pos+var-s)
   (let rec ((clause* clause*)
	     (pos 0))
     (let ((clause (source-code clause*)))
       (if (null? clause)
	   (values '()
		   '())
	   (let-pair
	    ((a* r*) clause)
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
 ;; sensible usage would probably remove the head first, though.
 > (values->vector (extract-variables '(list a b c)))
 #((list a b c)
   ())
 > (values->vector (extract-variables '(list a ,b ,c)))
 #((list a b c)
   ((2 . b) (3 . c)))
 )

(define clause->check
  (lambda (V clause-test clause-body remainder)
    (match*
     clause-test
     ((head . rest)
      (letv ((clause-code-rest pos+vars) (extract-variables rest))
	    `(let ,(map (lambda (pos+var)
			  ;; (XX: potential for optimization of list extraction)
			  (let-pair ((pos var) pos+var)
				    `(,var (list-ref ,V ,pos))))
			pos+vars)
	       ;; (of course, potential for optimizing accesses
	       ;; (constructions) here too, especially the pos'ed
	       ;; fields)
	       (if (equal? (,head ,@clause-code-rest) ,V)
		   (begin
		     ,@clause-body)
		   ,remainder)))))))

(TEST
 > (clause->check 'MYV '(list a ,b c) '((run b)) 'REM)
 (let ((b (list-ref MYV 1))) (if (equal? (list a b c) MYV) (begin (run b)) REM))
 )

(define clause:test
  (lambda (clause)
    (match* clause
	    ((test . body)
	     test))))

(define clause:testhead-xsym
  (lambda (clause)
    (assert* pair? (clause:test clause)
	     (lambda (test)
	       (assert* symbol? (car test)
			identity)))))

(define handle-op-group
  (lambda (input group)
    ;; XX assumes that there is only one group; should move V* V LEN up
    (let ((constructor (clause:testhead-xsym (source-code (car group)))))
      (symbol-case*
       constructor
       ((list)
	;; XX later: reuse the same code for 'apply list'
	(let ((groups (list-group-by group
				     (on (compose* length
						   source-code
						   clause:test)
					 <))))
	  (with-gensyms
	   (V* V LEN)
	   `(let* ((,V* ,input)
		   (,V (source-code ,V*))
		   (,LEN (improper-length ,V)))
	      (if (negative? ,LEN)
		  (source-error
		   ,V* "matching improper list not yet implemented")
		  (case ,LEN
		    ,@(map
		       (lambda (group)
			 (let ((len (improper-length
				     ;; ^ only to handle error myself
				     (source-code
				      (clause:test (car group))))))
			   (if (negative? len)
			       (source-error
				(car group) ;; well any..
				"invalid function application (improper list)")
			       ;; (XX no apply yet, so: )
			       (let ((resultlen (dec len)))
				 `((,resultlen)
				   ;; for now, only recognize
				   ;; unquotes in the top level
				   ;; of clause-test
				   ,(fold-right
				     (lambda (clause rem)
				       (match*
					clause
					((test . body)
					 (clause->check V ;; not V* !
							test
							body
							rem))))
				     `(source-error ,V* "no match")
				     group))))))
		       groups)
		    (else
		     (source-error ,V* "no match"))))))))
       (else
	(source-error constructor
		      "currently, only list is supported"))))))

(define-macro* (match input . clauses)
  ;; group according to type of datum
  (with-exception-handler
   ##primordial-exception-handler
   (thunk
    (let* ((clausegroups
	    (list-group-by clauses
			   (on clause:testhead-xsym
			       symbol<?))))
      (case (length clausegroups)
	((1)
	 (handle-op-group input (car clausegroups)))
	((0)
	 (source-error stx "missing clauses"))
	(else
	 (source-error stx "match currently only implements list matching")))))))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#match foo ((list something)))
 (let* ((GEN:6483 foo)
	(GEN:6484 (source-code GEN:6483))
	(GEN:6485 (improper-length GEN:6484)))
   (if (negative? GEN:6485)
       (source-error GEN:6483 "matching improper list not yet implemented")
       (case GEN:6485
	 ((1)
	  (let ()
	    (if (equal? (list something) GEN:6484)
		(begin)
		(source-error GEN:6483 "no match"))))
	 (else (source-error GEN:6483 "no match")))))
 > (match '(1 2 3) ((list 1 2 3) 'found))
 found
 > (%try-syntax-error (match '(1 2) ((list 1 2 3) 'found)))
 #(source-error "no match" 1 2)
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
 #(source-error "no match" 1 3 3)
 > (define (t v) (match v
			((list 1 2) 'found12)
			((list 1 ,x) (list 'foundx x))
			((list 1 ,x 4) (list 'foundx4 x))
			((apply list ,x ,y ,r) (list 'foundr x y r))))
 > (t '(1 2 3))
 (foundr 1 2 (3))
 > (t '(1 2 3 4))
 (foundr 1 2 (3 4))
 > (t '(1 2 4 3))
 (foundr 1 2 (4 3))
 )
