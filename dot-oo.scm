
;; Principle is to shadow previous definitions of the generic, and the
;; last one falls back on previous definitions. Which at the end will
;; be one that throws an exception.

(define (dot-oo:no-method-found-for-generic genericname)
  (lambda (obj . rest)
    (error (string-append "no method found for generic "
			  (object->string genericname)
			  " for value:")
	   obj)))

(define (dot-oo:split-name sym)
  (let ((parts (string-split (symbol->string* sym) #\.)))
    (mcase (possibly-sourcify parts sym)
	   (`(`a `b)
	    (values (string->symbol a)
		    (string->symbol
		     ;; really prepend the dot? I'm still confused, torn.
		     (string-append "." b)))))))

(TEST
 > (values->vector (dot-oo:split-name 'foo.bar))
 #(foo .bar)
 )

(define (dot-oo:if-type-then-run-else-run type? then else)
  (lambda (first . rest)
    (if (type? first)
	(apply then first rest)
	(apply else first rest))))

(define define.-template
  (lambda (name expr)
    (letv ((typename genericname) (dot-oo:split-name name))
	  `(begin
	     (define ,name ,expr)
	     (define-if-not-defined ,genericname
	       (dot-oo:no-method-found-for-generic ',genericname))
	     (set! ,genericname
		   (dot-oo:if-type-then-run-else-run
		    ,(source.symbol-append typename '?)
		    ,name
		    ,genericname))))))

(define-macro* (define. first . rest)
  (mcase first
	 (symbol?
	  (define.-template first (xone rest)))
	 (pair?
	  (let ((first* (source-code first)))
	    (define.-template (car first*)
	      `(lambda ,(cdr first*)
		 ,@rest))))))

;; Note: this resolves the type predicate at loading time. Later
;; changes to it will not be picked up anymore.
;; Is this bad?

;; Redefinitions of the same method by using define. in the running
;; system will keep the previous definition chained in the generic;
;; apart from the type checking cost this won't have any effect though
;; (leaks anyway; ok interpreted code wouldn't).

(TEST
 > (define. (list.ref x y) (list-ref x y))
 ;; > (%try-error (.ref "foo" 1))
 ;; #(error "no method found for generic .ref for value:" "foo")
 ;; well, if this test suite is run again, it will find one.
 > (define. (string.ref x y) (string-ref x y))
 > (.ref "foo" 1)
 #\o
 > (.ref '(a b c) 1)
 b
 )



;; omit the |make-| prefix for the constructor name; use "." as
;; separator, and use |define.| to define all methods so that they
;; become part of generic super functions
(define-macro* (define-struct. name . defs)
  `(define-struct_ define. ,name
     ;; don't override constructor-name if provided by user
     ,@(if (memq constructor-name: (map source-code defs))
	   `()
	   `(constructor-name: ,name))
     separator: "."
     ,@defs))

