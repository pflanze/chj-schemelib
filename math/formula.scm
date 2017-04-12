;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cj-source-wraps source:symbol-append))

;; math formulas

(defenum associativity
  left right)

(defstruct formula-op
  #(symbol? name)
  #((maybe natural?) arity)
  #(rational? precedence-level) ;; higher means higher precedence
  #(boolean? associative?) ;; #f means, left-associative, #t means doesn't matter
  #(boolean? commutative?) ;; 'flippability'. unused
  #(associativity? associativity) ;; weird, seems we need that, too.
  )

(defmacro (defformula-op name . args)
  `(def ,(source:symbol-append 'formula- name)
	(formula-op ',name ,@args)))

(defformula-op + #f 10 #t #t 'left)
(defformula-op * #f 20 #t #t 'left)
(defformula-op - #f 10 #f #f 'left)
(defformula-op / #f 20 #f #f 'left);;?
(defformula-op ^  2 30 #f #f 'right)


;; AST:

(defstruct formula-opapplication
  #(formula-op? op)
  #((list-of formula-expr?) args))

(defstruct formula-functiondefinition
  #(symbol? name)
  #((list-of symbol?) vars)
  #(formula-expr? body))

(defstruct formula-constantdefinition
  #(symbol? name)
  #(formula-expr? body))

;; same as formula-opapplication except for the formatting..
(defstruct formula-functionapplication
  #(symbol? name) ;; no first class functions for now
  #((list-of formula-expr?) args))

(defstruct formula-constant
  value)

(defstruct formula-variable
  #(symbol? name))

(def formula-expr? (either formula-opapplication?
			   formula-functionapplication?
			   formula-constant?
			   formula-variable?))

;; only toplevel functions for now

(def formula-item? (either formula-expr?
			   formula-functiondefinition?
			   formula-constantdefinition?))


;; pretty print

;; (def (symbol.formula-string s)
;;      (list->string
;;       (map (lambda (c)
;; 	     (case c
;; 	       ((#\*) #\')
;; 	       (else
;; 		c)))
;; 	   (string->list (symbol.string s)))))
;; wrong, only at the end

(def (symbol.formula-string s)
     (let* ((s* (string->list (symbol.string s)))
	    (sr (reverse s*)))
       (if (or (< (length s*) 2)
	       (char=? (car s*) #\*))
	   (list->string s*)
	   
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
				  '())))))))))

(TEST
 > (symbol.formula-string 'f)
 "f"
 > (symbol.formula-string '*)
 "*"
 > (symbol.formula-string 'foo)
 "foo"
 > (symbol.formula-string 'foo*)
 "foo'"
 > (symbol.formula-string 'foo**)
 "foo''"
 > (symbol.formula-string 'foo*b*)
 "foo*b'"
 )


(defstruct formula-ctx
  #(formula-item? item)
  #(boolean? left-is-first-argument?))

;; ctx is a (maybe formula-ctx?) of the outer item.

(def. (formula-functiondefinition.string/ctx e ctx)
  (string-append (symbol.formula-string (.name e))
		 "("
		 (strings-join (map symbol.formula-string (.vars e)) ",")
		 ") = "
		 (.string/ctx (.body e)
			      (formula-ctx e #f))))

(def. (formula-constantdefinition.string/ctx e ctx)
  (string-append (symbol.formula-string (.name e))
		 " = "
		 (.string/ctx (.body e)
			      (formula-ctx e #f))))

(def. (formula-functionapplication.string/ctx e ctx)
  (string-append (symbol.formula-string (.name e))
		 "("
		 (strings-join
		  (map/iota (lambda (e* i)
			      (.string/ctx e* (formula-ctx e (zero? i))))
			    (.args e))
		  ",")
		 ")"))

(def. (formula-opapplication.string/ctx e ctx)
  (let* ((op (.op e))
	 (opstr (string-append " " (symbol.formula-string (.name op)) " "))
	 (numargs (length (.args e))))
    (cond ((= numargs 0)
	   (error "XXX"))
	  ((= numargs 1)
	   ;; fix it on the fly?
	   (if (eq? op formula-/)
	       (.string/ctx (formula-opapplication formula-/
						   (cons (formula-constant 1)
							 (.args e)))
			    ctx)
	       (error "don't know how to handle numargs 1 with op:" op)))
	  (else
	   (let* ((need-parens?
		   (if ctx
		       (let-formula-ctx
			((item left-is-first-argument?) ctx)
			(cond ((formula-expr? item)
			       (cond ((formula-opapplication? item)
				      (match-cmp
				       ((on (compose .precedence-level .op)
					    number-cmp)
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
	      (strings-join (map/iota (lambda (e* i)
					(.string/ctx e* (formula-ctx e (zero? i))))
				      (.args e))
			    opstr)))))))


(def. (formula-variable.string/ctx e ctx)
  (symbol.formula-string (.name e)))

(def. (formula-constant.string/ctx e ctx)
  ;; XX is scheme formatting ok?
  (object->string (.value e)))


(define (formula-def bs e*)
  (mcase bs
	 (symbol?
	  (let ((name (source-code bs)))
	    (mcase e*
		   (`(lambda `bs* `e**)
		    (formula-functiondefinition
		     name
		     bs*
		     (formula e**)))
		   (else
		    (formula-constantdefinition
		     name
		     (formula e*))))))
	 (pair?
	  (mcase bs
		 (`(`name . `vars) ;; XX only fixed args supported.
		  (formula-functiondefinition
		   name
		   vars
		   (formula e*)))))))

(define (formula e)
  (mcase e
	 (pair?
	  (mcase e
		 (`(define `bs `e*)
		  (formula-def bs e*))
		 (`(def `bs `e*)
		  (formula-def bs e*))
		 (`(lambda `bs `e*)
		  ;; HACK. pretend it's named f
		  (formula-def (cons 'f bs) e*))
		 (`(+ . `args)
		  (formula-opapplication formula-+ (map formula args)))
		 (`(- . `args)
		  (formula-opapplication formula-- (map formula args)))
		 (`(* . `args)
		  (formula-opapplication formula-* (map formula args)))
		 (`(/ . `args)
		  (formula-opapplication formula-/ (map formula args)))
		 (`(square `x)
		  (formula-opapplication formula-^
					 (list (formula x)
					       (formula-constant 2))))
		 (`(expt `x `y)
		  (formula-opapplication formula-^
					 (list (formula x)
					       (formula y))))
		 (else
		  (mcase e
			 (`(`f . `args)
			  (mcase f
				 (symbol?
				  (formula-functionapplication f (map formula args)))))))))
	 (number?
	  (formula-constant e))
	 (symbol?
	  (formula-variable e))))

(TEST
 > (formula '(+ 1 2))
 #(formula-opapplication
   #(formula-op + #f 10 #t #t left)
   (#(formula-constant 1) #(formula-constant 2)))
 )


(define pp-formula (compose (C .string/ctx _ #f) formula))

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
 )



;; nice-wrappers:

(def (pp-formula-symbol #(symbol? sym))
     (let ((v (eval sym)))
       (if (procedure? v)
	   (pp-formula `(def ,sym ,(##decompile v)))
	   (pp-formula `(def ,sym ,v)))))

(def (pp-formulas syms)
     (for-each println (map pp-formula-symbol syms)))

