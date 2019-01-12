;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


"math formulas: currently just an AST with conversion from Scheme
sexpr, and to \"math\" string.
"

(require easy
	 (cj-source-wraps source:symbol-append)
	 (warn-plus WARN-ONCE))

(export pp-formula
	;; nice-wrappers:
	pp-formula-symbol
	pp-formulas

	#!optional
	(enum associativity)
	(class formula-op)
	
	;; AST:
	(class formula-ctx)
	(interface formula-item
		   (interface formula-expr
			      (class formula-opapplication)
			      (class formula-functionapplication)
			      (class formula-constant)
			      (class formula-variable))
		   (class formula-functiondefinition)
		   (class formula-constantdefinition))
	
	symbol.formula-string
	def->formula
	sexpr.formula ;; scheme-sexpr -> formula-AST
	)


;; Metainformation about ops for parsing

(defenum associativity
  left right)

(defclass (formula-op [symbol? name]
		      [(maybe natural?) arity]
		      ;; higher means higher precedence:
		      [rational? precedence-level]
		      ;; #f means, left-associative, #t means doesn't matter:
		      [boolean? associative?]
		      ;; 'flippability'. unused. :
		      [boolean? commutative?]
		      ;; weird, seems we need that, too:
		      [associativity? associativity]))


(defmacro (defformula-op name . args)
  `(def ,(source:symbol-append 'formula- name)
	(formula-op ',name ,@args)))

(defformula-op + #f 10 #t #t 'left)
(defformula-op * #f 20 #t #t 'left)
(defformula-op - #f 10 #f #f 'left)
(defformula-op / #f 20 #f #f 'left);;?
(defformula-op ^  2 30 #f #f 'right)


;; AST


(defclass (formula-ctx [formula-item? item]
		       [boolean? left-is-first-argument?]))


;; only toplevel functions for now
(definterface formula-item

  ;; (ctx is really optional: used for calculating need-parens?, which
  ;; is just #f if missing)
  (method (string/ctx e [(maybe formula-ctx?) ctx]))

  
  (definterface formula-expr

    (defclass (formula-opapplication [formula-op? op]
				     [(list-of formula-expr?) args])
      "application of an operator"

      (defmethod (string/ctx e ctx)
	(let* ((op (.op e))
	       (opstr (string-append " "
				     (symbol.formula-string (.name op))
				     " "))
	       (numargs (length (.args e))))
	  (cond ((= numargs 0)
		 (error "XXX"))
		((= numargs 1)
		 ;; fix it on the fly?
		 (if (eq? op formula-/)
		     (.string/ctx (formula-opapplication
				   formula-/
				   (cons (formula-constant 1)
					 (.args e)))
				  ctx)
		     (error "don't know how to handle numargs 1 with op:" op)))
		(else
		 (let* ((need-parens?
			 (if ctx
			     (let-formula-ctx
			      ((item left-is-first-argument?) ctx)
			      (cond
			       ((formula-expr? item)
				(cond
				 ((formula-opapplication? item)
				  (match-cmp
				   ((on (compose-function .precedence-level .op)
					real-cmp)
				    item
				    e)
				   ((lt) #f)
				   ((eq)
				    (if left-is-first-argument?
					(not (associativity-eq?
					      (.associativity (.op item))
					      'left))
					(not (.associative? (.op item)))))
				   ((gt) #t)))
				 ((formula-functionapplication? item)
				  #f)
				 ((formula-constant? item)
				  #f)
				 ((formula-variable? item)
				  #f)
				 (else
				  (error 'missing-case))))
			       ((formula-functiondefinition? item)
				#f)
			       (else 
				(error 'missing-case))))
			     #f))
			(perhaps-wrap (lambda (x)
					(if need-parens?
					    (string-append "(" x ")")
					    x))))
		   (perhaps-wrap
		    (strings-join
		     (map/iota (lambda (e* i)
				 (.string/ctx e* (formula-ctx e (zero? i))))
			       (.args e))
		     opstr))))))))

    ;; same as formula-opapplication except for the formatting..
    (defclass (formula-functionapplication [symbol? name]
					   ;; ^ no first class functions for now
					   [(list-of formula-expr?) args])
      "application of a function (not operator)"

      (defmethod (string/ctx e ctx)
	(string-append (symbol.formula-string (.name e))
		       "("
		       (strings-join
			(map/iota (lambda (e* i)
				    (.string/ctx e* (formula-ctx e (zero? i))))
				  (.args e))
			",")
		       ")")))

    (defclass (formula-constant value)

      (defmethod (string/ctx e ctx)
	;; XX is scheme formatting ok?
	(object->string (.value e))))

    (defclass (formula-variable [symbol? name])

      (defmethod (string/ctx e ctx)
	(symbol.formula-string (.name e)))))
  

  (defclass (formula-functiondefinition [symbol? name]
					[(list-of symbol?) vars]
					[formula-expr? body])

    (defmethod (string/ctx e ctx)
      (string-append (symbol.formula-string (.name e))
		     "("
		     (strings-join (map symbol.formula-string (.vars e)) ",")
		     ") = "
		     (.string/ctx (.body e)
				  (formula-ctx e #f)))))

  (defclass (formula-constantdefinition [symbol? name]
					[formula-expr? body])

    (defmethod (string/ctx e ctx)
      (string-append (symbol.formula-string (.name e))
		     " = "
		     (.string/ctx (.body e)
				  (formula-ctx e #f))))))


;; pretty print

(def (symbol.formula-string s)
     (let* ((s-str (symbol.string s))
	    (s-len (string.length s-str))
	    (s* (string->list s-str))
	    (sr (reverse s*)))

       ;; current rules rather random / ad-hoc, ok?
       (cond ((= s-len 0) (error "dunno how to format")) ;; ok?
	     ((= s-len 1) s-str)
	     ((char=? (car s*) #\*) s-str)
	     (else
	      (let lp ((s '())
		       (sr sr))
		(if (null? sr)
		    (list->string s)
		    (let-pair ((c sr*) sr)
			      (case c
				((#\*)
				 (lp (cons #\' s)
				     sr*))
				(else
				 ;; end special processing
				 (lp (rappend sr s)
				     '()))))))))))

(TEST
 > (symbol.formula-string 'f)
 "f"
 > (symbol.formula-string '*)
 "*"
 > (symbol.formula-string 'foo)
 "foo"
 > (symbol.formula-string '*foo*)
 "*foo*"
 > (symbol.formula-string 'foo*)
 "foo'"
 > (symbol.formula-string 'foo**)
 "foo''"
 > (symbol.formula-string 'foo*b*)
 "foo*b'")



(define (def->formula bs e*)
  (mcase bs
	 (symbol?
	  (let ((name (source-code bs)))
	    (define (lamb bs* e**)
	      (formula-functiondefinition name
					  bs*
					  (sexpr.formula e**)))
	    (mcase e*
	 	   (`(lambda `bs* `e**) (lamb bs* e**))
		   (`(##lambda `bs* `e**) (lamb bs* e**))
	 	   (else
	 	    (formula-constantdefinition name
						(sexpr.formula e*))))))
	 (pair?
	  (mcase bs
		 (`(`name . `vars) ;; XX only fixed args supported.
		  (formula-functiondefinition
		   name
		   vars
		   (sexpr.formula e*)))))))

(define (sexpr.formula e)
  (mcase e
	 (pair?
	  (mcase e
		 (`(define `bs `e*)
		  (def->formula bs e*))
		 (`(def `bs `e*)
		  (def->formula bs e*))
		 (`(lambda `bs `e*)
		  (WARN-ONCE "does this happen?")
		  ;; HACK: fake name
		  (def->formula (cons 'ANON bs) e*))
		 (`(##lambda `bs `e*)
		  (WARN-ONCE "does this happen?")
		  ;; HACK: fake name
		  (def->formula (cons 'ANON bs) e*))
		 (`(+ . `args)
		  (formula-opapplication formula-+ (map sexpr.formula args)))
		 (`(- . `args)
		  (formula-opapplication formula-- (map sexpr.formula args)))
		 (`(* . `args)
		  (formula-opapplication formula-* (map sexpr.formula args)))
		 (`(/ . `args)
		  (formula-opapplication formula-/ (map sexpr.formula args)))
		 (`(square `x)
		  (formula-opapplication formula-^
					 (list (sexpr.formula x)
					       (formula-constant 2))))
		 (`(expt `x `y)
		  (formula-opapplication formula-^
					 (list (sexpr.formula x)
					       (sexpr.formula y))))
		 (else
		  (mcase e
			 (`(`f . `args)
			  (mcase f
				 (symbol?
				  (formula-functionapplication
				   f
				   (map sexpr.formula args)))))))))
	 (number?
	  (formula-constant e))
	 (symbol?
	  (formula-variable e))))

(TEST
 > (sexpr.formula '(+ 1 2))
 [(formula-opapplication)
  [(formula-op) + #f 10 #t #t left]
  ([(formula-constant) 1] [(formula-constant) 2])])


(define pp-formula (compose-function (C .string/ctx _ #f) sexpr.formula))

(TEST
 > (pp-formula '1)
 "1"
 > (pp-formula 'x)
 "x"
 > (pp-formula '(+ (- 2 1) (/ 3 2)))
 "2 - 1 + 3 / 2"
 > (pp-formula '(- 5 (- 2 1) (/ 3 2)))
 "5 - (2 - 1) - 3 / 2"
 > (pp-formula '(- (- 2 1) (/ 3 2)))
 "2 - 1 - 3 / 2" ;; case where left-is-first-argument? comes into play
 > (pp-formula '(- (- 2 1) (/ (+ 3 3) 2)))
 "2 - 1 - (3 + 3) / 2"
 > (pp-formula '(- (- 2 1) (- 5 4)))
 "2 - 1 - (5 - 4)"
 ;; just a couple more. Systematic, please?
 > (pp-formula '(- 5 (- 2 1) (- 3 2)))
 "5 - (2 - 1) - (3 - 2)"
 > (pp-formula '(+ 5 (- 2 1) (- 3 2)))
 "5 + 2 - 1 + 3 - 2"
 > (pp-formula '(+ 5 (- 2 1) (/ 3 2)))
 "5 + 2 - 1 + 3 / 2"

 > (pp-formula '(* (+ (- 2 1) (/ 3 2)) x))
 "(2 - 1 + 3 / 2) * x"
 > (pp-formula '(sin (f x)))
 "sin(f(x))"
 > (pp-formula '(sin x))
 "sin(x)"
 > (pp-formula '(sin 3))
 "sin(3)"
 > (pp-formula '(sin (square x)))
 "sin(x ^ 2)"
 > (pp-formula '(sin (square (+ x 1))))
 "sin((x + 1) ^ 2)"
 > (pp-formula '(expt 2 3))
 "2 ^ 3"
 > (pp-formula '(expt (square x) -1/3))
 "(x ^ 2) ^ -1/3"
 > (pp-formula '(define (f x y) (+ (square x) y)))
 "f(x,y) = x ^ 2 + y"
 > (pp-formula `(def sq (lambda (x) (* x x))))
 "sq(x) = x * x"
 > (pp-formula `(def sq (##lambda (x) (* x x))))
 "sq(x) = x * x")


;; nice-wrappers:

(def (pp-formula-symbol [symbol? sym])
     (let ((v (eval sym)))
       (if (procedure? v)
	   (pp-formula `(def ,sym ,(decompile v)))
	   (pp-formula `(def ,sym ,v)))))

(def (pp-formulas syms)
     (for-each println (map pp-formula-symbol syms)))


(TEST
 > (pp-formula-symbol 'square)
 "square(x) = x * x")
