
(require (cj-env *do-times) ;;   keyword->symbol i've recreated here
	 ;;keyword-util does not have a module file.
	 (srfi-1 reverse!))

(export keyed->sxml
	sxml->keyed)


'(description "some tools for dealing with SXML")
'(doc "NOTE that the <DSSSL-like> format as used or produced
  by keyed->sxml and sxml->keyed is not really that (at least not
  as implemented by gambit's keyword function arguments):
  for this module, keyword/value pairs can be put anywhere
  in a list, not only at the end. Since in XML and SXML attributes are
  put near the start tag (\"function name\"), this is allowed here as
  well. But that might make sml->keyed unsuitable for production of
  function code. Maybe more functions for dealing with this will
  follow in the future.")

(include "cj-standarddeclares.scm")


(define (null-list? v)
  (cond ((pair? v) #f)
	((null? v) #t)
	(else (error "type error, expected list:" v))))

(define (@keyword->symbol v)
  (##string->symbol (##keyword->string v)))

(define (keyed->sxml lis)
  '(desc "turn a tree using DSSSL keyword parameter style to SXML (@ ..) association list style. "
	 "The output does not share pairs with the input.")
  '(args (lis pair "tree using DSSSL keyword parameter style."))
  '(return list "SXML tree")

  (if (pair? lis) ;; this check should be handled by the type framework
      ;; Timings using the test below have shown that using iteration
      ;; and set-cdr! (with everything in the loop, no set! to the
      ;; outside, that would really spoil performance) is only
      ;; marginally faster than the second-fastest variant using
      ;; iteration and reverse!. Using non-destructive reverse is
      ;; third, using recursion with manual cps style is forth, using
      ;; recursion with values and call-with-current-continuation is
      ;; last by *far*. I'm settling on reverse!. Note that copying
      ;; reverse! from srfi-1 to here would make a speedup of 20%!
      (let ((head (car lis)))
	(let iter ((l (cdr lis))
		   (attrs '())
		   (args '()))
	  (if (null-list? l)
	      (cons head (if (null? attrs)
			     (reverse! args)
			     (cons (cons '@ (reverse! attrs)) (reverse! args))))
	      (let ((v (car l))
		    (r (cdr l)))
		(if (keyword? v)
		    (if (pair? r)
			(iter (cdr r)
			      (cons (list (@keyword->symbol v) (car r)) attrs)
			      args)
			(error "keyed->sxml: missing value after keyword argument:"
			       lis))
		    (iter r
			  attrs
			  (cons (if (pair? v)
				    (keyed->sxml v)
				    v) args)))))))
      (error "keyed->sxml: expected non-null list:" lis)))


(define-macro (hide . rest)
  '(begin)) ;; we have to hide the docs here since the define
	    ;; afterwards (or other stuff like defines) wouldn't work.

(define (sxml->keyed lis)
  (hide
   '(desc "turn an SXML tree to DSSSL keyword parameter style. "
	  "NOTE that this is experimental and may loose some information. "
	  ;; "The output may share pairs with the input." -- not
	  ;; currently as map is used.
	  "NOTE: non-sxml lists around sxml tags are not supported yet."
	  )
   '(args (lis pair "SXML tree"))
   '(return list "tree in keyword parameter style")
   )
  
  (define (map-rest lis)
    (map (lambda(v)
	   (if (pair? v)
	       (sxml->keyed v)
	       ;; ^ XX: non-sxml lists around sxml tags not supported
	       ;; yet.  And btw: should I still support giving atoms
	       ;; to the conversion functions as well?
	       v))
	 lis))
  (if (pair? lis)
      (if (pair? (cdr lis))
	  (let ((head (car lis))
		(maybe-attrs (cadr lis)))
	    (cons head
		  (if (and (pair? maybe-attrs)
			   (eq? (car maybe-attrs) '@))
		      ;; turn ((key val)..) to key: val ..  todo: iirc
		      ;; SXML attr lists can have more than one value
		      ;; per key (sublists can be longer than length
		      ;; 2), so the conversion will be lossy. check
		      ;; what we'll be missing.
		      (let recur ((l (cdr maybe-attrs)))
			(if (null-list? l)
			    ;; process subtags in the body of the tag:
			    (map-rest (cddr lis))
			    (let ((v (car l)))
			      (if (and (pair? v)
				       (pair? (cdr v)))
				  (let ((key (car v))
					(val (cadr v)))
				    (cons (symbol->keyword key)
					  (cons val (recur (cdr l)))))
				  (error "sxml->keyed: entry in attribute list is not a list of length >=2:"
					 v)))))
		      (map-rest (cdr lis)))))
	  lis)
      (error "sxml->keyed: expected non-null list:" lis)))

