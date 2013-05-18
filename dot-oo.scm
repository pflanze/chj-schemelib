(require cj-typed ;; heh indirectly through define-struct. expansion
	 )


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

(both-times
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
		(strings-join (cdr parts) ":") str))))))

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

(both-times
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
			 ,genericname)))))))))

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
;; become part of generic super functions. Also, allow field typing.

(define define-struct/types:arg->maybe-fieldname
  (lambda (v*)
    (let ((v (source-code v*)))
      (cond ((symbol? v)
	     v*)
	    ((typed? v)
	     (typed.var v))
	    ((meta-object? v)
	     #f)
	    (else
	     (source-error
	      v*
	      "expecting symbol or typed symbol or meta-object"))))))


(both-times
 (define dot.oo:have-no-typecheck
   '(symbol? fixnum? string? boolean? number? integer? complex?)))

(define-macro* (define-struct. name . defs)
  (with-gensyms
   (V V*)
   `(begin
      ,(apply define-struct-expand
	      'define.
	      'typed-lambda
	      define-struct/types:arg->maybe-fieldname
	      (lambda (var field+)
		(let ((field+* (source-code field+)))
		  (if (typed? field+*)
		      (begin
			(assert (= (vector-length field+*) 2))
			(vector (vector-ref field+* 0)
				var))
		      var)))
	      (lambda (FN field+)
		(let ((field+* (source-code field+)))
		  (if (typed? field+*)
		      (begin
			(assert (= (vector-length field+*) 2))
			(with-gensyms
			 (V V*)
			 `(lambda (,V)
			    (let ((,V* (,FN ,V)))
			      ;; (XX btw much code duplication? (of the
			      ;; type check code, in case it is big))
			      (type-check ,(vector-ref field+* 0) ,V*
					  ,V*)))))
		      FN)))
	      name
	      separator: "."
	      ;; don't override constructor-name if provided by user
	      (if (memq constructor-name: (map source-code defs))
		  defs
		  `(constructor-name: ,name ,@defs)))
      ;; reserve name for just this purpose?
      (define. (,(source.symbol-append name ".typecheck!") ,V)
	,@(filter values
		  (map (lambda (def)
			 (let ((def* (source-code def)))
			   (if (typed? def*)
			       `(let ((,V*
				       ;; use accessors, or direct vector-ref? :
				       (,(source.symbol-append name "." (vector-ref def* 1)) ,V)))
				  (type-check ,(vector-ref def* 0)
					      ,V*
					      ;; stupid (begin) in if not allowed:
					      (void))
				  ;; recursion
				  ,@(if (memq (source-code (vector-ref def* 0))
					      dot.oo:have-no-typecheck)
					`()
					`((.typecheck! ,V*))))
			       ;; NOTE: doen't recurse for fields
			       ;; without type
			       ;; declaration. Hm. (Because I would
			       ;; still need typecheck! methods for
			       ;; basic types then; and more over,
			       ;; maybe some vector in those fields
			       ;; with symbol head is just a vector
			       ;; with symbol head?)
			       #f)))
		       defs))
	;; and stupid (begin) analogon not allowed again:
	(void)))))


;; need this often enough I guess:
(define nothing? not)
(define. (nothing.typecheck! x)
  (void))

;; hmm. inefficiency worries except doesn't matter for typecheck; but
;; also ugly? (But wouldn't special-casing list-of in
;; define-struct. be ugler?)
(define. (list.typecheck! l)
  (for-each .typecheck! l))

;; hm even special-casing doesn't help in all cases, thus:
(define (void/1 x) (void))
(define. boolean.typecheck! void/1)
(define. number.typecheck! void/1) ;; heh includes fixnum etc. of course
(define. symbol.typecheck! void/1)
(define. string.typecheck! void/1)


(TEST
 > (define-struct. foo #(fixnum? x))
 > (%try-error (foo 'a))
 #(error "does not match fixnum?:" a)
 > (foo 10)
 #(foo 10)
 > (.x #)
 10
 > (define-struct. foo #!key #(integer? x) #(boolean? b))
 > (foo x: 10 b: #t)
 #(foo 10 #t)
 > (foo b: #t x: 10)
 #(foo 10 #t)
 > (.x-set # 12)
 #(foo 12 #t)
 > (%try-error (.x-set # 'n))
 #(error "does not match integer?:" n)
 > (.x-update '#(foo 10 #t) inc)
 #(foo 11 #t)
 > (%try-error (.x-update '#(foo 10 #t) true/1))
 #(error "does not match integer?:" #t)
 > (%try-error (foo b: 11 x: 10))
 #(error "does not match boolean?:" 11)
 ;; > (define-struct. foo #(integer? x) #!optional (b #t))
 ;; "expecting symbol or typed symbol or meta-object" XX hmm, still not complete.

 ;; typecheck feature
 > (define-struct. foo #!key x #(boolean? b))
 > (.typecheck! '#(foo #(foo 10 #f) #f))
 > (%try-error (.typecheck! '#(foo #(foo 10 #f) 12)))
 #(error "does not match boolean?:" 12)
 ;; > (%try-error (.typecheck! '#(foo #(foo 10 11) #f)))
 ;; was hoping for an error here. But, see NOTE in the source.
 > (define-struct. foo #!key #((either foo? nothing?) x) #(boolean? b))
 > (.typecheck! '#(foo #(foo #f #f) #f))
 > (%try-error (.typecheck! '#(foo #(foo #f 11) #f)))
 #(error "does not match boolean?:" 11)
 )

