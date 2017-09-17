;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 cj-phasing
	 cj-functional
	 (srfi-11 fst snd)
	 cj-source
	 cj-source-2
	 cj-symbol
	 list-util-2
	 cj-alist
	 cj-struct
	 (string-util strings-join)
	 C)



;; destructuring bind

;; (list 1 2 x ,x) matches `(1 2 ,x ,X) with anything in X, binding x
;; to what X is

;; can handle source code

;; XXX: inefficiencies:
;; - improper-length walks even in rest arg match cases
;; ...

;; clause 'data type':

(both-times
 
 (define clause:parse
   (lambda (clause)
     (match* clause
	     ((test . body)
	      (values test body)))))

 (define clause:test (compose fst clause:parse))
 (define clause:body (compose snd clause:parse))

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
	       len))))))

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

(both-times
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

(both-times
 
 (define cj-match:equal? source-equal?)

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
	    (if (cj-match:equal? ,(cj-possibly-sourcify-deep
				   `(,@(if apply? '(apply) '())
				     ,constructor
				     ,@clause-code-rest)
				   (clause:test clause)) ,V)
		(begin
		  ,@(clause:body clause))
		,remainder)))))))

(TEST
 > (clause->check '((list a ,b c) (run b)) 'MYV 'REM)
 (let ((b (list-ref MYV 1)))
   (if (cj-match:equal? (list a b c) MYV) (begin (run b)) REM))
 )

(both-times

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
	   (= inpa clausea)))))

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

(both-times
 (define handle-op-group
   (lambda (input opgroup message-string)
     ;; XX assumes that there is only one group; should move V* V LEN up
     (let ((constructor (clause:constructor-xsym (source-code (car opgroup)))))
       (symbol-case
	constructor
	((list)
	 ;; XX later: reuse the same code for 'apply list'
	 (let* ((grouped-by-nargs
		 (segregate opgroup (on clause:test-nargs <))))
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
					 (source-error ,V* ,message-string))))
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
				  `(source-error ,V* ,message-string)
				  (filter clause:apply?
					  opgroup))))))))))))
	(else
	 (source-error constructor
		       "only list matching is implemented")))))))


(both-times
 (define (match-expand stx input clauses message-string)
   ;; group according to type of datum
   (let* ((clausegroups
	   (segregate clauses
		      (on clause:constructor-xsym
			  symbol<?))))
     (case (length clausegroups)
       ((1)
	(handle-op-group input (car clausegroups) message-string))
       ((0)
	(source-error stx "missing clauses"))
       (else
	(source-error stx "match currently only implements list matching"))))))

(define-macro*d (match input . clauses)
  (match-expand stx input clauses "no match"))

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
			((apply list ,x ,y ,r) (list 'foundr x y r))))
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


;; Wrapper that goes back to a more 'traditional' syntax (?):

(both-times

 ;; can't use improper-fold-right* since (unquote x) and (quasiquote x) can
 ;; be in tail position.

 (define (maybe-quasiquote-or-unquote e*
				      #!optional
				      quasiquote-match?
				      unquote-match?)
   (let ((e (source-code e*)))
     ;; (use and shortcutting, thanks to *-less mixmatching of those)
     (and (pair? e)
	  (let-pair
	   ((a* r*) e)
	   (let* ((a (source-code a*))
		  (r (source-code r*))
		  (test (lambda (maybe-match?)
			  (and
			   (or (not maybe-match?)
			       (maybe-match? (source-code (car r))))
			   (values a
				   (car r))))))
	     (and (symbol? a)
		  (pair? r)
		  (null? (cdr r))
		  (case a
		    ((quasiquote)
		     (test quasiquote-match?))
		    ((unquote)
		     (test unquote-match?))
		    (else
		     #f))))))))

 (TEST
  > (values->vector (maybe-quasiquote-or-unquote '`a))
  #(quasiquote a)
  > (values->vector (maybe-quasiquote-or-unquote '``a))
  #(quasiquote `a) ;; correct, even though it's not a quasiquoted variable
  > (values->vector (maybe-quasiquote-or-unquote '`a symbol?))
  #(quasiquote a)
  > (maybe-quasiquote-or-unquote '`a string?)
  #f
  > (maybe-quasiquote-or-unquote '``a symbol?)
  #f
  > (values->vector (maybe-quasiquote-or-unquote ',b))
  #(unquote b)
  > (maybe-quasiquote-or-unquote ''b)
  #f
  > (maybe-quasiquote-or-unquote '(unquote b c))
  #f
  > (maybe-quasiquote-or-unquote '(unquote b . c))
  #f
  > (values->vector (maybe-quasiquote-or-unquote ',`b))
  #(unquote `b)
  > (values->vector (maybe-quasiquote-or-unquote ',`b symbol?))
  #(unquote `b)
  ;; I actually decided to give the unquote checker separately.
  > (maybe-quasiquote-or-unquote ',`b #f symbol?)
  #f
  > (values->vector (maybe-quasiquote-or-unquote ',b #f symbol?))
  #(unquote b)
  )
 
 (define add-quote
   (lambda (e*)
     (list 'quote e*)))
 
 (define changequote
   (lambda-values
    ((which var))
    (symbol-case which
		 ((unquote)
		  ;; let it evaluate
		  var)
		 ((quote)
		  ;; hm just double quote it, I guess?
		  (add-quote var))
		 ((quasiquote)
		  ;; var is already checked to be a symbol.  bind to
		  ;; variable
		  (list 'unquote var))
		 (else
		  (add-quote var)))))

 (define change-expr
   (lambda (e)
     (cond ((maybe-quasiquote-or-unquote e symbol?)
	    => changequote)
	   (else
	    (add-quote e)))))
 
 (define matchl-test->match-test
   (lambda (test)
     (let* ((proper
	     (lambda (r)
	       `(list ,@r)))
	    (improper
	     (lambda (r)
	       `(apply list ,@r)))
	    )
       (letv ((improper? test*)
	      (let rec ((cl test))
		(cond ((maybe-quasiquote-or-unquote cl symbol?)
		       => (compose (lambda (r)
				     (values #t
					     (list r)))
				   changequote))
		      (else
		       (let ((cl* (source-code cl)))
			 (cond ((pair? cl*)
				(let-pair ((a r) cl*)
					  (letv ((improper? r) (rec r))
						(values improper?
							(cons (change-expr a)
							      r)))))
			       ((null? cl*)
				(values #f
					'()))
			       (else
				(source-error
				 test
				 "impossible to create such input with list"))))))))
	     ((if improper? improper proper) test*)))))

 )

(TEST
 > (change-expr 'a)
 'a
 > (change-expr ',a)
 a
 > (change-expr '`a)
 ,a

 > (matchl-test->match-test '(a b c))
 (list 'a 'b 'c)
 > (%try-syntax-error (matchl-test->match-test '(a b . c)))
 #(source-error "impossible to create such input with list")
 > (matchl-test->match-test ',a)
 (apply list a)
 > (matchl-test->match-test '(a ,b . `c))
 (apply list 'a b ,c)

 > (matchl-test->match-test '``a)
 (list 'quasiquote ,a)
 )

(both-times
 (define (matchl-expand stx input clauses message-string)
   (match-expand
    stx
    input
    (map (lambda (clause)
	   (match clause
		  ((apply list ,test ,body)
		   `(,(matchl-test->match-test test)
		     ,@body))))
	 clauses)
    message-string)))

(define-macro*d (matchl input . clauses)
  (matchl-expand stx input clauses "no match"))

(TEST
 > (matchl '(a b)
	       ((a b) 1))
 1
 > (matchl '(a c)
	       ((a b) 1)
	       ((a `b) b))
 c
 > (matchl '(a c) ((a b) 1) ((a `b . `c) (vector b c)))
 #(c ())
 > (matchl '(a c f) ((a b) 1) ((a `b . `c) (vector b c)))
 #(c (f))
 > (define b 'c)
 > (matchl '(a c f) ((a b) 1) ((a ,b . `c) c))
 (f)
 > (define b 'd)
 > (%try-syntax-error (matchl '(a c f) ((a b) 1) ((a ,b . `c) c)))
 #(source-error "no match")
 )


;; also check and dispatch for other types than lists:

(both-times
 (define-struct mcaseclauses
   list
   other
   else ;; always last ? need ?
   )

 ;; XX todo: use these in actual matcher, too, right?
 (define (mcaseclauses-list-matchexpressions c)
   (let-mcaseclauses
    ((l o e) c)
    (map (lambda (cl)
	   ;; (let* ((c* (car (source-code cl)))
	   ;; 	  (c (source-code c*)))
	   ;;   (if (and (pair? c)
	   ;; 	      (eq? (source-code (car c)) 'quasiquote)
	   ;; 	      (= (length c) 2))
	   ;; 	 (cadr c)
	   ;; 	 (source-error c* "not quasiquote, BUG?")))

	   ;; but then actually realizing that I probably want it
	   ;; *with* the quasiquote...

	   (car (source-code cl)))
	 l)))

 ;; XX todo dito.
 ;; for predicate based matches
 (define (mcaseclauses-other-matchexpressions c)
   (let-mcaseclauses
    ((l o e) c)
    (map (lambda (cl)
	   (car (source-code cl)))
	 o)))

 ;; there is no mcaseclauses-else-matchexpressions, stupid.

 ;; string describing what is expected:
 (define (mcaseclauses-message c)
   (let ((es
	  ;; XX which ones are to be checked first? todo:
	  ;; do same as in matcher, i.e. provide method.
	  (append (mcaseclauses-other-matchexpressions c)
		  (mcaseclauses-list-matchexpressions c))))
     (if (null? es)
	 ;; no match clauses! (should we throw an error at compile
	 ;; time instead?)
	 "can never match"

	 (string-append
	  "no match, expecting: "
	  (strings-join (map (lambda (m)
			       (object->string (cj-desourcify m)))
			     es)
			" | ")))))

 (define mcase-no-clauses (make-mcaseclauses '()
					     '()
					     '()))

 (define (mcase-separate-clauses clauses)
   (if (pair? clauses)
       (let-pair ((clause clauses*) clauses)
		 (define (rec)
		   (mcase-separate-clauses clauses*))
		 (let ()
		   (define (cont updater)
		     (updater (rec)
			      (C cons clause _)))
		   (matchl clause
			   ((else . `body)
			    (cont mcaseclauses-else-update))
			   ((`expr . `body)
			    (if (pair? (source-code expr))
				(matchl expr
					((quasiquote `expr)
					 (cont mcaseclauses-list-update))
					(`_
					 (cont mcaseclauses-other-update)))
				(cont mcaseclauses-other-update)))
			   (`_
			    (source-error clause
					  "invalid mcase form")))))
       mcase-no-clauses))

 (TEST
  > (mcase-separate-clauses '((else 1) ((identity symbol?) 2) (`(`x) 3) (else 4) (symbol? 5)))
  #((mcaseclauses)
    ((`(`x) 3))
    (((identity symbol?) 2) (symbol? 5))
    ((else 1) (else 4)))
  ))

(define-macro*d (mcase expr . clauses)
  (let* ((sepclauses (mcase-separate-clauses clauses))
	 (message-string (mcaseclauses-message sepclauses)))
    (with-gensyms
     (V V* ELSE _)
     `(let* ((,V ,expr)
	     ,@(if (not (null? (mcaseclauses-other sepclauses)))
		   `((,V* (source-code ,V)))
		   ;; XX: warning: predicates will get the data with
		   ;; source annotation removed from only the top
		   ;; level! Deep removal would be costly and
		   ;; *usually* not necessary, better solution would be
		   ;; to encode this knowledge in source-aware
		   ;; predicates
		   `())
	     (,ELSE
	      (lambda ()
		,(matchl (mcaseclauses-else sepclauses)
			 ((`elseclause)
			  (matchl elseclause
				  ((else `what)
				   what)))
			 (()
			  `(source-error ,V ,message-string))
			 (`_
			  (source-error stx "more than one else clause"))))))
	(cond
	 ;; XX: ordering of list vs other (vs else) is thrown away here, bad?

	 ;; other (first, because thought to be more efficient?)
	 ,@(map (lambda (clause)
		  (matchl clause
			  ((`pred . `body)
			   `((,pred ,V*)
			     ,@body))))
		(mcaseclauses-other sepclauses))

	 ;; list
	 ,@(if (not (null? (mcaseclauses-list sepclauses)))
	       `(((natural0? (improper-length (source-code ,V)))
		  ;;^ XX assumes that there are no annotated pairs further behind
		  ,(matchl-expand
		    stx
		    V
		    (append (map (lambda (clause)
				   (matchl clause
					   ((`quotedform . `body)
					    (matchl quotedform
						    ((quasiquote `form)
						     (cons form body))))))
				 (mcaseclauses-list sepclauses))
			    (if* (mcaseclauses-else sepclauses)
				 ;; `(`,,'_ (,ELSE))
				 (list
				  (list (list 'quasiquote _)
					(list ELSE)))
				 '()))
		    message-string)))
	       '())

	 ;; else
	 (else
	  (,ELSE)))))))

(TEST
 > (%try-syntax-error (mcase 1))
 #(source-error "can never match")
 > (mcase 1 (even? 'even) (odd? 'odd))
 odd
 > (mcase 2 (even? 'even) (odd? 'odd))
 even
 ;; > (mcase '(a) (even? 'even) (odd? 'odd) (else 'nomatch))
 ;; *** ERROR IN (console)@62.1 -- (Argument 1) INTEGER expected
 ;; (even? '(a))
 ;; ah hm
 > (mcase '(a) (number? 'num) (else 'nomatch))
 nomatch
 > (mcase '(a) (number? 'num) (`(a) 'lis) (else 'nomatch))
 lis
 > (%try-syntax-error (mcase '(b) (number? 'num) (`(a) 'lis)))
 #(source-error "no match, expecting: number? | `(a)")
 > (%try-syntax-error (mcase '(b) (number? 'num) (`(a) 'lis) (else 'nomatch)))
 nomatch
 > (mcase '(a) (number? 'num) (`(`a) 'lis) (else 'nomatch))
 lis

 ;; else handling:
 > (define TEST:equal? syntax-equal?)
 ;; > (expansion#mcase 'a (`(a b) 1) (foo? 3) (else 2))
 ;; (let* ((GEN:V1903 'a)
 ;; 	(GEN:V*1904 (source-code GEN:V1903))
 ;; 	(GEN:ELSE1905 (lambda () 2)))
 ;;   (cond ((foo? GEN:V*1904) 3)
 ;; 	 ((natural0? (improper-length (source-code GEN:V1903)))
 ;; 	  (matchl GEN:V1903 ((a b) 1) (`GEN:_1906 (GEN:ELSE1905))))
 ;; 	 (else (GEN:ELSE1905))))
 ;;it's more complicated now since it expands matchl in place.
 > (mcase 'a (`(a b) 1) (symbol? 3) (else 2))
 3
 > (mcase '"a" (`(a b) 1) (symbol? 3) (else 2))
 2
 > (mcase '() (`(a b) 1) (symbol? 3) (else 2))
 2
 )

;; quasiquote and unquote (and quote) matching:

(TEST
 ;; quote
 
 > (mcase '(quote x) (`(quote `y) y))
 x
 > (mcase '(quote x) (`(quote x) 'yes) (else 'no))
 yes
 > (mcase '(quote x) (`(quote y) x) (else 'no))
 no
 ;; verbatim match because not var inside:
 > (mcase '(quote x) (`(quote `'y) y) (else 'no))
 no
 > (mcase '(quote `'y) (`(quote `'y) 'y) (else 'no))
 y

 ;; quasiquote
 
 > (mcase '(quasiquote x) (`(quasiquote `y) y))
 x
 ;; top level (improper list) quasiquote match
 > (mcase '(quasiquote x) (`(quasiquote y) y))
 `x
 ;; dito improper list
 > (mcase '(foo quasiquote x) (`(foo quasiquote y) y))
 `x
 ;; first quasiquote is verbatim
 > (mcase '(foo quasiquote x) (`(foo quasiquote `y) y))
 x
 > (%try-syntax-error (mcase '(foo quasiquotee x) (`(foo quasiquote `y) y)))
 #(source-error "no match, expecting: `(foo quasiquote `y)")

 ;; unquote

 > (mcase '(unquote* x) (`(unquote* `y) y))
 x
 ;; > (mcase '(unquote x) (`(unquote `y) y))
 ;; *** ERROR IN #<procedure #8>, (console)@64.1 -- (Argument 2) LIST expected
 ;; (apply '#<procedure #2 list> 'y)
 ;;XXX fix
 ;; gmb turns `y into 'y, right? btw why list ?
 
 )

(TEST ;; error messages
 > (%try-syntax-error (mcase 1 (`(`a `b) a)))
 #(source-error "no match, expecting: `(`a `b)")
 > (%try-syntax-error (mcase 1 (pair? a)))
 #(source-error "no match, expecting: pair?")
 > (%try-syntax-error (mcase 1 (`(`a `b) a) (pair? a)))
 #(source-error "no match, expecting: pair? | `(`a `b)"))


;; like match-lambda (?):
(define-macro* (mcase-lambda . clauses)
  (with-gensyms
   (V)
   `(lambda (,V)
      (mcase ,V
	     ,@clauses))))

