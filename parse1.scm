(require easy
	 jclass
	 (list-util-3 list-starts-with? char-list-starts-with-string?)
	 debuggable-promise
	 (oo-util-lazy iseq?)
	 (oo-util char-list.show char-list+?)
	 test
	 char-util
	 )

(export
 (macro PARSE1) ;; to get all of the below without the prefix
 
 ;; Parsers
 parse1#at-end?
 parse1#point
 parse1#rest
 parse1#whitespace
 parse1#whitespace*
 parse1#whitespace+

 ;; Parser generators
 parse1#anything
 parse1#char-of-class
 parse1#capture-char-of-class
 parse1#match-list?
 parse1#match-list
 parse1#match-string?
 parse1#match-string

 parse1#match-pred
 parse1#match*-pred
 parse1#match+-pred

 parse1#match-while
 parse1#capture-while
 parse1#maybe-capture-until

 ;; Parser combinators
 parse1#either
 parse1#>>
 (macro parse1#mdo)
 parse1#any
 parse1#many
 parse1#repeat

 ;; Parser combinators and extractors for capture:
 parse1#capture
 parse1#>>=
 parse1#return
 (macro parse1#mlet*)
 (macro parse1#mlet)
 (macro parse1#mIf))


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


(defmacro (PARSE1 . body)
  `(##let ()
	  (##namespace ("parse1#"
			;; Parsers
			at-end?
			point
			rest
			whitespace
			whitespace*
			whitespace+

			;; Parser generators
			anything
			char-of-class
			capture-char-of-class
			match-list?
			match-list
			match-string?
			match-string
			match-pred
			match*-pred
			match+-pred
			match-while
			capture-while
			maybe-capture-until

			;; Parser combinators
			either
			>>
			mdo
			any
			many
			repeat

			;; Parser combinators and extractors for capture:
			capture
			>>=
			return
			mlet*
			mlet
			mIf))
	  ,@body))


;; A non-capturing parser is a function taking a sequence and
;; returning the remainder of the sequence after the match.

;; A capturing parser is a function taking a sequence and returning
;; the parse1#capture-while (which could be anything, for example a subsequence,
;; boolean, ...) and the remainder of the sequence after the capture.

;; A parser failure is reported as a call to (parse1-backtrack),
;; which exits the current continuation. (parse1-backtrack) takes
;; a message string and any number of other arguments deemed useful
;; for error reporting.

;; Most of the functions exported from this module are not parsers
;; themselves, but return parsers. Parser generators are functions
;; that take configuration arguments, parser combinators are those
;; that take other parsers as arguments.


(defparameter parse1:current-backtrack (lambda (e)
					 ((current-exception-handler) e)))

(def (parse1-error e)
     ((parse1:current-backtrack) e))

(def (on-parse1-error continue thunk)
     (continuation-capture
      (lambda (cont)
	(parameterize
	 ((parse1:current-backtrack
	   (lambda (e)
	     (continuation-graft
	      cont
	      continue e))))
	 (thunk)))))



(jclass parse1-failure

	(jclass (list-match-failure #(iseq? match)
				    #(iseq? at-input)
				    #(iseq? input))
		(def-method* (message _)
		  "input does not start with string"))

	(jclass (all-options-failure failures #(iseq? input))
		(def-method* (message _)
		  "none of the options matched"))

	(jclass (char-class-match-failure #(char-list+? chars) #(iseq? input))
		(def-method* (message _)
		  "input does not start with a char out of"))

	(jclass (repeat-failure #(exact-natural0? n)
				#(exact-natural0? m)
				#(exact-natural0? i)
				#(parse1-failure? failure)
				#(iseq? input))
		(def-method* (message _)
		  "match should repeat n..m times but fails on the i-th repetition"))

	(jclass (match-pred-failure #(function? pred) #(iseq? input))
		(def-method* (message _)
		  "failure expecting an item satisfying pred"))

	(jclass parse1-unexpected-eof

		(jclass (char-class-unexpected-eof #(char-list+? chars))
			(def-method* (message _)
			  "unexpected end of input while expecting a char out of"))

		(jclass (match-pred-unexpected-eof #(function? pred))
			(def-method* (message _)
			  "unexpected end of input while expecting an item satisfying pred"))))


(def parse1:non-capturing-result?
     iseq?)

(def parse1:boolean-capturing-result?
     (values-of boolean? iseq?))

(def parse1:any-capturing-result?
     (values-of any? iseq?))

(def parse1:char-capturing-result?
     (values-of char? iseq?))

(def parse1:non-capturing-or-any-capturing-result?
     (either parse1:non-capturing-result?
	     parse1:any-capturing-result?))

(def parse1:input-capturing-result?
     (values-of iseq? iseq?))


(def parse1:non-capturing-parser? procedure?)
(def parse1:any-capturing-parser? procedure?)
(def parse1:non-capturing-or-any-capturing-parser? procedure?)
;; a function returning a parser
(def parse1:->non-capturing-or-any-capturing-parser? procedure?)


;; Parsers

(def (parse1#at-end? #(iseq? l))
     -> parse1:boolean-capturing-result?
     (values (null? l) l))

(def (parse1#point #(iseq? l))
     -> parse1:input-capturing-result?
     (values l l))

(def (parse1#rest #(iseq? l))
     -> parse1:input-capturing-result?
     (values l '()))

(def (parse1#anything #(iseq? l))
     -> parse1:non-capturing-result?
     (cdr l))


;; Parser generators

(def ((parse1#char-of-class #(char-list+? chars))
      #(iseq? l))
     -> parse1:non-capturing-result?
     (if (null? l)
	 (parse1-error (char-class-unexpected-eof chars))
	 (let-pair ((a l*) l)
		   (if (memq a chars)
		       l*
		       (parse1-error (char-class-match-failure chars l))))))

;; while parse1#capture could be used with parse1:char-of-class, the
;; following avoids a bit of overhead and will be a tad simpler to
;; write. Premature opt?
(def ((parse1#capture-char-of-class #(char-list+? chars))
      #(isqe? l))
     -> parse1:char-capturing-result?
     (if (null? l)
	 (parse1-error (char-class-unexpected-eof chars))
	 (let-pair ((a l*) l)
		   (if (memq a chars)
		       (values a l*)
		       (parse1-error (char-class-match-failure chars l))))))


(def ((parse1#match-list? #(iseq? templ))
      #(iseq? l))
     -> parse1:boolean-capturing-result?
     (list-starts-with? l templ))

(def ((parse1#match-list #(iseq? templ))
      #(iseq? l))
     -> iseq?
     (letv ((b l*) (list-starts-with? l templ))
	   (if b l*
	       (parse1-error (list-match-failure templ l* l)))))


(def ((parse1#match-string? #(string? templ))
      #(iseq? l))
     -> parse1:boolean-capturing-result?
     (char-list-starts-with-string? l templ))

(def ((parse1#match-string #(string? templ))
      #(iseq? l))
     -> iseq?
     (letv ((b l*) (char-list-starts-with-string? l templ))
	   (if b l*
	       (parse1-error (list-match-failure templ l* l)))))


(def ((parse1#match-pred #(function? pred))
      #(iseq? l))
     -> iseq?
     (if (null? l)
	 (parse1-error (match-pred-unexpected-eof pred))
	 (let-pair ((a l*) l)
		   (if (pred a)
		       l*
		       (parse1-error (match-pred-failure pred l))))))

(def ((parse1#match*-pred #(function? pred))
      #(iseq? l))
     -> iseq?
     (let lp ((l l))
       (if (null? l)
	   l
	   (let-pair ((a l*) l)
		     (if (pred a)
			 (lp l*)
			 l)))))

(def (parse1#match+-pred #(function? pred))
     (parse1#mdo (parse1#match-pred pred)
		 (parse1#match*-pred pred)))


(def ((parse1#match-while pred) l)
     -> parse1:non-capturing-result?
     (drop-while pred l))


(def ((parse1#capture-while pred) l)
     -> (values-of iseq? iseq?)
     (values (take-while pred l)
	     (drop-while pred l)))


(def ((parse1#maybe-capture-until pred skip-end-marker?) l)
     -> (values-of (maybe iseq?) iseq?)
     (cond ((find-tail pred l)
	    => (lambda (tail)
		 (values (take-while (complement pred) l)
			 (if skip-end-marker?
			     (cdr tail)
			     tail))))
	   (else
	    (values #f
		    l))))



;; Parser combinators

(def ((parse1#>> #(parse1:non-capturing-parser? p1)
		 #(parse1:non-capturing-or-any-capturing-parser? p2))
      #(iseq? l))
     -> parse1:non-capturing-or-any-capturing-result?
     ;; ^ which one is determined by parser2 (which could be a
     ;; narrower type than any?, though)

     (p2 (p1 l)))


(defmacro (parse1#mdo . parser-exprs)
  (if (one? parser-exprs)
      ;; this would not call parse1#>> at all, and hence not do early
      ;; type checking, hence: (XX: can this be turned off via
      ;; cj-typed-disable ? Probably not. Solve.)
      (let ((e (car parser-exprs)))
	(cj-sourcify-deep
	 `(-> parse1:non-capturing-or-any-capturing-parser?
	      ,e)
	 e))
      `(LA parse1#>> ,@parser-exprs)))


;; XX tempted to do this via binary operator and |LA|, too, but then
;; how to detect the end, means, each binary operator would capture
;; the error and wrap it, unlike once for all of them. Although to be
;; fair, if the user nests manually, the same thing happens. But then
;; perhaps that's what the user expects to be reported. So, leave via
;; run-time n-ary function for now.

(def ((parse1#either . #((list-of parse1:non-capturing-or-any-capturing-parser?)
			ps))
      #(iseq? l))
     -> parse1:non-capturing-or-any-capturing-result?
     ;; ^ same as the widest return type of all the ps

     (let lp ((ps ps)
	      (failures '()))
       (if (null? ps)
	   (parse1-error (all-options-failure failures l))
	   (let-pair ((p ps*) ps)
		     (on-parse1-error
		      (lambda (e) (lp ps* (cons e failures)))
		      (& (p l)))))))





;; (def ((parse1#any parser) l)
;;      (let lp ((l l))
;;        (on-parse1-error
;; 	(lambda (e) l)
;; 	(& (lp (parser l))))))

;; Allocates closure and uses call/cc and parameterize on each
;; character. Can only avoid this by way of mutation, or perhaps by
;; moving to a CPS model. I.e. hard to optimize automatically? Thus
;; somewhat unsafe hand-optimzation via mutation:

;; like "*" in regexes; parser must be non-capturing
(def ((parse1#any #(parse1:non-capturing-parser? p))
      #(iseq? l))
     (on-parse1-error
      (lambda (e) l)
      (& 
       (let lp ()
	 (set! l (p l))
	 (lp)))))

;; like "+" in regexes; parser must be non-capturing
(def (parse1#many #(parse1:non-capturing-parser? p))
     (parse1#mdo p
		 (parse1#any p)))

;; like "{n,m}" in regexes, n and m are inclusive; parser must be
;; non-capturing
(def (parse1#repeat #(exact-natural0? n)
		    #(exact-natural0? m)
		    #(parse1:non-capturing-parser? parser))
     (assert (<= n m)) ;; or let it fail at parse time?
     (lambda (#(iseq? l))
       (def i 0)
       (on-parse1-error
	(lambda (e)
	  (if (fx<= i n)
	      (parse1-error
	       (repeat-failure n m i e l))
	      l))
	(&
	 (let lp ()
	   (if (fx< i m)
	       (begin
		 (set! i (fx+ i 1))
		 (set! l (parser l))
		 (lp))
	       l))))))

;; ^ XX add variants that return how many times they matched?


;; Parser combinators and extractors for capture:

(def ((parse1#capture #(parse1:non-capturing-parser? p))
      #(iseq? l))
     -> parse1:input-capturing-result?

     (let ((l* (p l)))
       (values (let rec ((l l))
		 (if (or (null? l) (eq? l l*))
		     '()
		     (let-pair ((a r) l)
			       (cons a (rec r)))))
	       l*)))



(TEST
 > (.show ((PARSE1 (capture (match-list (.list "foo"))))
	   (.list "foo bar")))
 (values (.list "foo") (.list " bar"))

 > (%try ((PARSE1 (capture (match-list (.list "foob"))))
	  (.list "foo baz")))
 (exception text: "This object was raised: #((list-match-failure) (#\\f #\\o #\\o #\\b) (#\\space #\\b #\\a #\\z) (#\\f #\\o #\\o #\\space #\\b #\\a #\\z))\n")
  
 > (def p (PARSE1 (capture (either (match-list (.list "foo"))
				   (match-list (.list "bar"))))))
 > (.show (p (.list "foo baz")))
 (values (.list "foo") (.list " baz"))
 > (.show (p (.list "bar baz")))
 (values (.list "bar") (.list " baz"))
 > (with-exception-catcher .show (& (p (.list "buz baz"))))
 (all-options-failure
  (list (list-match-failure (.list "bar") (.list "uz baz") (.list "buz baz"))
	(list-match-failure (.list "foo") (.list "buz baz") (.list "buz baz")))
  (.list "buz baz"))
 
 > (def p (PARSE1 (capture
		   (mdo
		    (either (match-list (.list "H"))
			    (match-list (.list "h")))
		    (match-list (.list "ello World"))))))
 > (.show (p (.list "Hello World!")))
 (values (.list "Hello World") (.list "!"))
 > (.show (p (.list "hello World!")))
 (values (.list "hello World") (.list "!"))
 > (with-exception-catcher .show (& (p (.list "hello world!"))))
 (list-match-failure (.list "ello World")
		     (.list "world!")
		     (.list "ello world!")))


;; Capturing intermediate results:

(def ((parse1#>>= #(parse1:any-capturing-parser? p)
		  #(parse1:->non-capturing-or-any-capturing-parser? receiver))
      #(iseq? l))
     (letv ((val l*) (p l))
	   ((receiver val) l*)))


;; ... and injecting them:

(def ((parse1#return val) #(iseq? l))
     (values val l))



;; XX should use generic monad syntax... improvement even if
;; parametrized!

(defmacro (parse1#mlet* binds . body)
  (assert*
   pair-or-null? binds
   (lambda (binds*)
     (let rec ((binds binds*))
       (if (null? binds)
	   `(parse1#mdo ,@body)
	   (let-pair ((bind binds*) binds)
		     (mcase bind
			    (`(`var `parser-expr)
			     (cj-sourcify-deep
			      `(parse1#>>= ,parser-expr
					   (lambda (,var)
					     ,(rec binds*)))
			      ;; *slightly* evil as the bind
			      ;; expression isn't (directly) the one
			      ;; doing the call, but, better than
			      ;; using mlet's context (and using
			      ;; parser-expr's location would be
			      ;; wrong):
			      bind)))))))))

(defmacro (parse1#mlet binds . body)
  (assert*
   pair-or-null? binds
   (lambda (binds*)
     (let rec ((binds binds*)
	       (vars (map (mcase-lambda
			   (`(`v `parser-expr)
			    (gensym (source-code v))))
			  binds*)))
       (if (null? binds)
	   `(parse1#mdo ,@body)
	   (let*-pair (((bind binds*) binds)
		       ((var vars*) vars))
		      (mcase bind
			     (`(`var `parser-expr)
			      (cj-sourcify-deep
			       `(parse1#>>= ,parser-expr
					    (lambda (,var)
					      ,(rec binds*
						    vars*)))
			       ;; dito (see mlet*)
			       bind)))))))))



(TEST
 > (.show ((PARSE1 (mlet ((v (capture-while char-alpha?)))
			 (match-list (.list " W"))
			 (char-of-class '(#\r #\o #\d #\l))
			 (return (.string (cons #\¿ v)))))
	   (.list "Hello Worlds!")))
 (values "¿Hello" (.list "rlds!"))

 ;; accept multiple instances of the char class:
 > (.show ((PARSE1 (mlet ((v (capture-while char-alpha?)))
			 (match-list (.list " W"))
			 (any (char-of-class '(#\r #\o #\d #\l)))
			 ;; (^ XX could optimize)
			 (return (.string (cons #\¿ v)))))
	   (.list "Hello Worlds!")))
 (values "¿Hello" (.list "s!"))

 > (def (p n m)
	(comp* .show
	       (PARSE1 (capture (repeat n m (char-of-class (.list "abcde")))))
	       .list))
 > ((p 0 0) "ab")
 (values (list) (.list "ab"))
 > ((p 0 1) "ab")
 (values (.list "a") (.list "b"))
 > ((p 1 1) "ab")
 (values (.list "a") (.list "b"))
 > ((p 0 2) "ab")
 (values (.list "ab") (list))
 > ((p 1 2) "ab")
 (values (.list "ab") (list))
 > ((p 2 2) "ab")
 (values (.list "ab") (list))
 ;; eof
 > ((p 2 2000000) "ab")
 (values (.list "ab") (list))
 ;; submatch failure
 > ((p 0 2) "abxy")
 (values (.list "ab") (.list "xy"))
 > ((p 1 3) "abxy")
 (values (.list "ab") (.list "xy"))
 > ((p 2 4) "abxy")
 (values (.list "ab") (.list "xy"))
 > (with-exception-catcher .show (& ((p 3 4) "abxy")))
 (repeat-failure 3 4
		 3
		 (char-class-match-failure (.list "abcde") (.list "xy"))
		 (.list "xy"))


 > (def p (comp* .show
		 (PARSE1 (capture (many (char-of-class (.list "ab")))))
		 .list))
 > (p "abcd")
 (values (.list "ab") (.list "cd"))
 > (p "axbcd")
 (values (.list "a") (.list "xbcd"))
 > (with-exception-catcher .show (& (p "xbcd")))
 ;; even though this is in parse1:many, only report parse1#char-of-class
 ;; failure? XX add wrapper?
 (char-class-match-failure (.list "ab") (.list "xbcd"))
 
 
 > (.show ((PARSE1
	    (mlet* ((a (capture-while char-alpha?))
		    (b (capture-while char-numeric?)))
		   (match-list (.list " W"))
		   (return (values (.string (cons #\¿ a))
				   (.string b)))))
	   (.list "Hello31 Worlds!")))
 (values (values "¿Hello" "31") (.list "orlds!"))
 )


(defmacro (parse1#mIf test-parser then-parser else-parser)
  (with-gensym
   V
   `(parse1#mlet ((,V ,test-parser))
		(If ,V
		    ,then-parser
		    ,else-parser))))


(TEST
 > (def (p rest-or-point)
	(comp*
	 .show
	 (PARSE1
	  (mlet ((num (capture (match-while char-numeric?))))
		(match-while char-whitespace?)
		(mIf at-end?
		     (return (.string num))
		     (mlet ((r rest-or-point))
			   (return (.string r))))))
	 .list))
 > ((p parse1#rest) "123")
 (values "123" (list))
 > ((p parse1#rest) "123 ")
 (values "123" (list))
 > ((p parse1#rest) "123 abc")
 (values "abc" (list))
 > ((p parse1#point) "123 abc")
 (values "abc" (.list "abc")))



;; Utilities

(def parse1#whitespace (PARSE1 (match-pred char-whitespace?)))
(def parse1#whitespace* (PARSE1 (any whitespace)))
(def parse1#whitespace+ (PARSE1 (many whitespace)))


(TEST
 > (def (p parser)
	(comp* .string
	       parser
	       .list))
 > (with-exception-catcher .show (& ((p (PARSE1 whitespace)) "Hello World")))
 (match-pred-failure char-whitespace? (.list "Hello World"))
 > ((p (PARSE1 whitespace)) " World")
 "World"
 > ((p (PARSE1 whitespace)) " \n\r \tWorld")
 "\n\r \tWorld"
 > (with-exception-catcher .show (& ((p (PARSE1 whitespace)) "")))
 (match-pred-unexpected-eof char-whitespace?)

 > ((p (PARSE1 whitespace*)) "Hello World")
 "Hello World"
 > ((p (PARSE1 whitespace*)) " World")
 "World"
 > ((p (PARSE1 whitespace*)) " \n\r \tWorld")
 "World"
 > ((p (PARSE1 whitespace*)) "")
 ""

 > (with-exception-catcher .show (& ((p (PARSE1 whitespace+)) "Hello World")))
 (match-pred-failure char-whitespace? (.list "Hello World"))
 > ((p (PARSE1 whitespace+)) " World")
 "World"
 > ((p (PARSE1 whitespace+)) " \n\r \tWorld")
 "World"
 > (with-exception-catcher .show (& ((p (PARSE1 whitespace+)) "")))
 (match-pred-unexpected-eof char-whitespace?))

