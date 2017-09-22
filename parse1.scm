(require easy
	 jclass
	 (list-util-3 list-starts-with? char-list-starts-with-string?)
	 debuggable-promise
	 (oo-util-lazy iseq?)
	 (oo-util char-list.show char-list+?)
	 (cj-port with-output-to-string)
	 (cj-exception-handler write-exception-message)
	 test
	 char-util
	 )

(export
 (macro PARSE1) ;; to get all of the below without the prefix
 
 ;; Parsers
 parse1#at-end?
 parse1#the-end
 parse1#point
 parse1#rest
 parse1#whitespace
 parse1#whitespace*
 parse1#whitespace+

 ;; Parser generators
 parse1#anything
 parse1#nothing
 parse1#char-of-class
 parse1#capture-char-of-class
 parse1#match-list?
 parse1#match-list
 parse1#match-string?
 parse1#match-string

 parse1#match-pred/desc (macro parse1#match-pred)
 parse1#match*-pred
 parse1#match+-pred/desc (macro parse1#match+-pred)

 parse1#match-while
 parse1#capture-while
 parse1#maybe-capture-until

 ;; Parser combinators
 parse1#optional
 parse1#meither
 parse1#>>
 (macro parse1#mdo)
 parse1#any
 parse1#many
 parse1#repeat
 parse1#capturing-any
 parse1#capturing-many
 parse1#capturing-repeat

 ;; Parser combinators and extractors for capturing
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
			the-end
			point
			rest
			whitespace
			whitespace*
			whitespace+

			;; Parser generators
			anything
			nothing
			char-of-class
			capture-char-of-class
			match-list?
			match-list
			match-string?
			match-string
			match-pred/desc match-pred
			match*-pred
			match+-pred/desc match+-pred
			match-while
			capture-while
			maybe-capture-until

			;; Parser combinators
			optional
			meither
			>>
			mdo
			any
			many
			repeat
			capturing-any
			capturing-many
			capturing-repeat

			;; Parser combinators and extractors for capture:
			capture
			>>=
			return
			mlet*
			mlet
			mIf))
	  (mdo ,@body)))


;; A non-capturing parser is a function taking a sequence and
;; returning the remainder of the sequence after the match.

;; A capturing parser is a function taking a sequence and returning
;; the capture (which could be anything, for example a subsequence,
;; boolean, ...) and the remainder of the sequence after the capture.

;; A parser failure is reported as a call to (parse1-backtrack),
;; which exits the current continuation. (parse1-backtrack) takes
;; a message string and any number of other arguments deemed useful
;; for error reporting.

;; Most of the functions exported from this module are not parsers
;; themselves, but return parsers. Parser generators are functions
;; that take configuration arguments, parser combinators are those
;; that take other parsers as arguments.


(defparameter parse1:current-backtrack
  (lambda (e)
    ;; there's no ##make-error-exception but could just (error
    ;; (.message-string e)). But then really go with typed exceptions
    ;; and instead turn them to .message-string or so upon entering
    ;; the repl.
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




(jinterface
 parse1-failure-interface

 ;; message to be fed to write-exception-message from
 ;; cj-exception-handler:
 (method (maybe-exception-message _) -> list?)
 ;; same as maybe-exception-message (which will always give a message
 ;; here, too):
 (method (exception-message _) -> list?)

 (jclass
  parse1-failure

  (def-method (maybe-exception-message v)
    (.exception-message v))

  (def-method (message-string v)
    (with-output-to-string
      (& (write-exception-message (.maybe-exception-message v)))))


  (jclass (list-match-failure #(iseq? match)
			      #(iseq? at-input)
			      #(iseq? input))
	  (def-method* (exception-message _)
	    (list "input does not start with list"
		  match: (.show match)
		  at-input: (.show at-input)
		  ;; ^ try to take position or something instead
		  input: (.show input))))

  (jclass (string-match-failure #(string? match)
				#(iseq? at-input)
				#(iseq? input))
	  (def-method* (exception-message _)
	    (list "input does not start with string"
		  match: match
		  at-input: (.show at-input)
		  ;; ^ try to take position or something instead
		  input: (.show input))))

  (jclass (all-options-failure failures #(iseq? input))
	  (def-method* (exception-message _)
	    (list "none of the options matched"
		  failures: (map .exception-message failures)
		  input: (.show input))))

  (jclass (char-class-match-failure #(char-list+? chars) #(iseq? input))
	  (def-method* (exception-message _)
	    (list "input does not start with a char out of"
		  chars: chars
		  input: (.show input))))

  (jclass (repeat-failure #(exact-natural0? n)
			  #(exact-natural0? m)
			  #(exact-natural0? i)
			  #(parse1-failure? failure)
			  #(iseq? input))
	  (def-method* (exception-message _)
	    (list "match should repeat n..m times but fails on the i-th repetition"
		  n: n m: m i: i
		  failure: (.exception-message failure)
		  input: (.show input))))

  (jclass (match-pred-failure #(function? pred) desc #(iseq? input))
	  (def-method* (exception-message _)
	    (list "failure expecting an item satisfying pred"
		  (if desc desc pred)
		  input: (.show input))))

  (jclass (expecting-eof-failure #(iseq? input))
	  (def-method* (exception-message _)
	    (list "expecting end of input"
		  input: (.show input))))
  
  (jclass parse1-unexpected-eof

	  (jclass (generic-unexpected-eof #(symbol? kind))
		  (def-method* (exception-message _)
		    (list "unexpected end of input while expecting"
			  kind)))

	  (jclass (char-class-unexpected-eof #(char-list+? chars))
		  (def-method* (exception-message _)
		    (list "unexpected end of input while expecting a char out of"
			  chars)))

	  (jclass (match-pred-unexpected-eof #(function? pred) desc)
		  (def-method* (exception-message _)
		    (list "unexpected end of input while expecting an item satisfying pred"
			  (or desc (.show pred))))))))


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

(def parse1:results-capturing-result?
     (values-of iseq?
		;; ^ (stream-of any?) (or (stream-of T?) where T? is
		;; the type returned by the parser parameter) instead
		;; of (stream-of T2?)  where T2? is the type of the
		;; input elements (often char?).
		iseq?))


(def parse1:non-capturing-parser? procedure?)
(def parse1:any-capturing-parser? procedure?)
(def parse1:non-capturing-or-any-capturing-parser? procedure?)
;; a function returning a parser
(def parse1:->non-capturing-or-any-capturing-parser? procedure?)



;; Macros put early so they can be used in "Parsers" section without
;; having to reorder everything (the procedures, >> and >>=, are
;; forward visible):

;; XX should use generic monad syntax... improvement even if
;; parametrized!

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



;; Parsers

;; returns whether we are at eof
(def (parse1#at-end? #(iseq? l))
     -> parse1:boolean-capturing-result?
     (values (null? l) l))

;; like "$" in regexes (fails unless at eof)
(def (parse1#the-end #(iseq? l))
     -> parse1:non-capturing-result?
     (if (null? l) l
	 (parse1-error (expecting-eof-failure l))))

;; returns the rest but does not consume it
(def (parse1#point #(iseq? l))
     -> parse1:input-capturing-result?
     (values l l))

;; consumes the rest
(def (parse1#rest #(iseq? l))
     -> parse1:input-capturing-result?
     (values l '()))

;; like "." in regexes
(def (parse1#anything #(iseq? l))
     -> parse1:non-capturing-result?
     (if (null? l)
	 (parse1-error (generic-unexpected-eof 'anything))
	 (cdr l)))

;; zero-width match (beware of endless loops!)
(def (parse1#nothing #(iseq? l))
     -> parse1:non-capturing-result?
     l)


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
      #(iseq? l))
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
	       (parse1-error (string-match-failure templ l* l)))))


(def ((parse1#match-pred/desc #(function? pred) desc)
      #(iseq? l))
     -> iseq?
     (if (null? l)
	 (parse1-error (match-pred-unexpected-eof pred desc))
	 (let-pair ((a l*) l)
		   (if (pred a)
		       l*
		       (parse1-error (match-pred-failure pred desc l))))))

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

(def (parse1#match+-pred/desc #(function? pred) desc)
     (parse1#mdo (parse1#match-pred/desc pred desc)
		 (parse1#match*-pred pred)))

(defmacro (parse1#match-pred arg) `(parse1#match-pred/desc ,arg ',arg))
(defmacro (parse1#match+-pred arg) `(parse1#match+-pred/desc ,arg ',arg))


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


(def ((parse1#optional #(parse1:non-capturing-parser? p))
      #(iseq? l))
     -> parse1:non-capturing-result?
     (on-parse1-error
      (lambda (e) l)
      (& (p l))))


(def ((parse1#>> #(parse1:non-capturing-parser? p1)
		 #(parse1:non-capturing-or-any-capturing-parser? p2))
      #(iseq? l))
     -> parse1:non-capturing-or-any-capturing-result?
     ;; ^ which one is determined by parser2 (which could be a
     ;; narrower type than any?, though)

     (p2 (p1 l)))

;; (Also see macro parse1#mdo, defined at the top for use in first
;; parser section.)


;; XX tempted to do this via binary operator and |LA|, too, but then
;; how to detect the end, means, each binary operator would capture
;; the error and wrap it, unlike once for all of them. Although to be
;; fair, if the user nests manually, the same thing happens. But then
;; perhaps that's what the user expects to be reported. So, leave via
;; run-time n-ary function for now.

(def ((parse1#meither . #((list-of parse1:non-capturing-or-any-capturing-parser?)
			 parsers))
      #(iseq? l))
     -> parse1:non-capturing-or-any-capturing-result?
     ;; ^ same as the widest return type of all the ps

     (let lp ((ps parsers)
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

;; Variants that capture a list of all results of the matches.

;; NOTE that with e.g.
;;
;;   (def P1 (capture (any p1)))
;;   (def P2 (capturing-any p2))
;;
;; P1 is *not* the same as P2. p1 must be a non-capturing parser, P1
;; captures a list of the input items. p2 must be a capturing parser,
;; P2 returns a list of all the results returned by p2.

(def (parse1#capturing-any #(parse1:any-capturing-parser? p)
			   #!optional
			   (#(iseq? tail) '()))
     (named rec
	    (lambda (#(iseq? l)) -> parse1:results-capturing-result?
	       ;; not optimized here, will be slowish
	       (on-parse1-error
		(lambda (e) (values tail l))
		(& 
		 (let*-values (((v l*) (p l))
			       ((vs l**) (rec l*)))
		   (values (cons v vs)
			   l**)))))))


(def (parse1#capturing-many #(parse1:any-capturing-parser? p)
			    #!optional
			    (#(iseq? tail) '()))
     (PARSE1 (mlet ((v0 p)
		    (vs (any p tail)))
		   (return (cons v0 vs)))))


(def (parse1#capturing-repeat #(exact-natural0? n)
			      #(exact-natural0? m)
			      #(parse1:any-capturing-parser? p)
			      #!optional
			      (#(iseq? tail) '()))
     (assert (<= n m)) ;; or let it fail at parse time?
     (lambda (#(iseq? l)) -> parse1:results-capturing-result?

	(def (parse/ rec l i)
	     (let*-values (((v l*) (p l))
			   ((vs l**) (rec l* (fx+ i 1))))
	       (values (cons v vs)
		       l**)))

	(let rec ((l l)
		  (i 0))
	  (if (< i n)
	      (parse/ rec l i)

	      ;; Entering the part that does only stop, not backtrack
	      ;; upon failure.  Unlike in parse1#repeat, to allow for
	      ;; lazy results in the future, do not optimize here?
	      (let rec ((l l)
			(i i))
		(if (< i m)
		    (on-parse1-error
		     (lambda (e) (values tail l))
		     (& (parse/ rec l i)))
		    
		    (values tail l)))))))



;; Parser combinators and extractors for capturing:

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
  
 > (def p (PARSE1 (capture (meither (match-list (.list "foo"))
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
		    (meither (match-list (.list "H"))
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


;; (Also see mlet and mlet* defined at the top.)


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
 > (with-exception-catcher .message-string
			   (& ((p (PARSE1 whitespace)) "Hello World")))
 "failure expecting an item satisfying pred: char-whitespace? input: (.list \"Hello World\")"
  > ((p (PARSE1 whitespace)) " World")
 "World"
 > ((p (PARSE1 whitespace)) " \n\r \tWorld")
 "\n\r \tWorld"
 > (with-exception-catcher .message-string (& ((p (PARSE1 whitespace)) "")))
 "unexpected end of input while expecting an item satisfying pred: char-whitespace?"

 > ((p (PARSE1 whitespace*)) "Hello World")
 "Hello World"
 > ((p (PARSE1 whitespace*)) " World")
 "World"
 > ((p (PARSE1 whitespace*)) " \n\r \tWorld")
 "World"
 > ((p (PARSE1 whitespace*)) "")
 ""

 > (with-exception-catcher .message-string
			   (& ((p (PARSE1 whitespace+)) "Hello World")))
 "failure expecting an item satisfying pred: char-whitespace? input: (.list \"Hello World\")"
 > ((p (PARSE1 whitespace+)) " World")
 "World"
 > ((p (PARSE1 whitespace+)) " \n\r \tWorld")
 "World"
 > (with-exception-catcher .message-string (& ((p (PARSE1 whitespace+)) "")))
 "unexpected end of input while expecting an item satisfying pred: char-whitespace?")


(TEST
 > (def p (comp* .show (PARSE1 anything) .list))
 > (p "Hello")
 (.list "ello")
 > (p "o")
 (list)
 > (with-exception-catcher .show (& (p "")))
 (generic-unexpected-eof 'anything)
 > (.show ((PARSE1 nothing) (.list "foo")))
 (.list "foo")
 > (def p (comp* .show (PARSE1 (optional whitespace)
			       (optional (match-pred char-alpha?))
			       (optional (match-pred char-alpha?))) .list))
 > (p "Hello")
 (.list "llo")
 > (p " Hello")
 (.list "llo")
 > (p "H ello")
 (.list " ello")

 > (def p (comp* .show (PARSE1 (optional whitespace)
			       (mlet ((c (capture
					  (optional (match-pred char-alpha?)))))
				     (optional (match-pred char-alpha?))
				     the-end
				     (return c))) .list))
 > (with-exception-catcher .show (& (p "Hel")))
 (expecting-eof-failure (.list "l"))
 > (p " He")
 (values (.list "H") (list))
 > (p "He")
 (values (.list "H") (list))
 > (p "H")
 (values (.list "H") (list))
 > (p "")
 (values (list) (list))

 > (def p (comp* .show (PARSE1 (capturing-any
				(mdo whitespace+
				     (capture
				      (many (match-pred char-alpha?))))))
		 .list))
 > (p "")
 (values (list) (list))
 > (p " ")
 (values (list) (.list " "))
 > (p "a")
 (values (list) (.list "a"))
 > (p " abc0")
 (values (list (.list "abc"))
	 (.list "0"))
 > (p " abc def 0 ghi")
 (values (list (.list "abc") (.list "def"))
	 (.list " 0 ghi"))

 > (def p (comp* .show
		 (PARSE1
		  (capturing-any
		   (mdo whitespace+
			(capturing-repeat 2 4
					  (capture (match-pred char-alpha?))))))
		 .list))
 > (p "")
 (values (list) (list))
 > (p " ")
 (values (list) (.list " "))
 > (p "a")
 (values (list) (.list "a"))
 > (p " abc0")
 (values (list (list (.list "a") (.list "b") (.list "c")))
	 (.list "0"))
 > (p " abc def 0 ghi")
 (values (list (list (.list "a") (.list "b") (.list "c"))
	       (list (.list "d") (.list "e") (.list "f")))
	 (.list " 0 ghi"))
 > (p " ab0")
 (values (list (list (.list "a") (.list "b"))) (.list "0"))
 > (p " abcdef0")
 (values (list (list (.list "a") (.list "b") (.list "c") (.list "d")))
	 (.list "ef0"))
 > (p " abcd ef0")
 (values (list (list (.list "a") (.list "b") (.list "c") (.list "d"))
	       (list (.list "e") (.list "f")))
	 (.list "0")))
