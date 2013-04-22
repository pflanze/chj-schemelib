
;; Principle is to shadow previous definitions of the generic, and the
;; last one falls back on previous definitions. Which at the end will
;; be one that throws an exception.

(define (dot-oo:no-method-found-for-generic genericname)
  (lambda (obj . rest)
    (error (string-append "no method found for generic "
			  (object->string genericname)
			  " for value:")
	   obj)))

;; (XX lost the string-split that allowed to give me limits on the
;; number of result values. Working around using strings-join.)

(define (dot-oo:split-typename.methodname str)
  (let ((parts (string-split (source-code str) #\.)))
    (mcase (possibly-sourcify parts str)
	   (`(`typename . `rest)
	    (values typename
		    ;; really prepend the dot? I'm still confused, torn.
		    (string-append "." (strings-join rest ".")))))))

(define (dot-oo:split-prefix:typename.methodname str)
  (define (cont prefix remainder)
    (letv ((typename methodname) (dot-oo:split-typename.methodname remainder))
	  (values prefix
		  typename
		  methodname)))
  (let* ((parts (string-split (source-code str) #\:)))
    (if (= (length parts) 1)
	(cont "" str)
	(cont (string-append (car parts) ":")
	      (possibly-sourcify
	       (strings-join (cdr parts) ":") str)))))

(TEST
 > (values->vector (dot-oo:split-typename.methodname "foo.bar"))
 #("foo" ".bar")
 > (values->vector (dot-oo:split-typename.methodname "foo.bar.baz"))
 #("foo" ".bar.baz")
 > (values->vector (dot-oo:split-typename.methodname "fix:foo.bar.baz"))
 #("fix:foo" ".bar.baz")
 > (values->vector (dot-oo:split-prefix:typename.methodname
		    "fix:foo.bar.baz:boo"))
 #("fix:" "foo" ".bar.baz:boo")
 > (values->vector (dot-oo:split-prefix:typename.methodname
		    "foo.bar.baz"))
 #("" "foo" ".bar.baz")
 ;; XX I'm not testing "foo.bar.baz:boo".. what should it do then?...
 )

(define (dot-oo:if-type-then-run-else-run type? then else)
  (lambda (first . rest)
    (if (type? first)
	(apply then first rest)
	(apply else first rest))))

(define define.-template
  (lambda (name expr)
    (let ((namestr (possibly-sourcify (symbol->string (source-code name))
				      name)))
      (letv ((prefixstr typenamestr genericnamestr)
	     (dot-oo:split-prefix:typename.methodname namestr))
	    (let ((genericname (string->symbol (string-append prefixstr
							      genericnamestr)))
		  (typename (string->symbol typenamestr)))
	      `(begin
		 (define ,name ,expr)
		 (define-if-not-defined ,genericname
		   (dot-oo:no-method-found-for-generic ',genericname))
		 (set! ,genericname
		       (dot-oo:if-type-then-run-else-run
			,(source.symbol-append typename '?)
			,name
			,genericname))))))))

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

