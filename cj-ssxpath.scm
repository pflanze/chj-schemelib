;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require list-util
	 stream
	 cj-sxml
	 cj-env  ;; natural0?, should be moved
	 cj-functional ;; compose
	 test
	 cj-typed
	 (oo-vector-lib strings-append))

(export ssxpath-matches ;; on single elements; auto-curring
	ssxpath-matches* ;; on lists of elements; auto-curring
	)


;; Simple XPath alike matching for SXML.

;; "./foo/bar[0]" == '(foo bar 0)
;; "./foo/bar[0]/baz" == '(foo bar 0 baz)
;; "./foo/*[0]/baz" == '(foo * 0 baz)

;; / for getting the contents of the elements that matched on the
;; previous level (implicit if the path has more entries)

;; @ for attribute matches, (* @ * /) returns all attribute values of
;; all tags in the current context.

;; sublists for "where" (or rather termed "having"?) matches on the
;; context before the sublist.

;; Does need a list or stream to be given, individual sxml-elements
;; are not ok.

(define (list-not-element? v)
  (FV (v)
      (if (pair? v)
	  (not (symbol? (car v)))
	  (null? v))))

(define-typed (_ssxpath-matches* path #(list-not-element? l) context)
  (if (null? path)
      l
      (let-pair
       ((pathhead path*) path)

       (let* ((bodies (stream-fold-right
		       (lambda (e res)
			 ;; need a body-append (or at least use
			 ;; stream-append) for cases where bodies are
			 ;; lazy.
			 (append (sxml-element-body e) res))
		       '()
		       l))
	      (lbodies (if (pair? context)
			   bodies
			   l))
	      (rec (lambda (l*)
		     (_ssxpath-matches* path*
					l*
					(cons pathhead context)))))
	 (cond ((eq? pathhead '/)
		(rec bodies))
	       ((eq? pathhead '*)
		(rec (stream-filter sxml-element? lbodies)))
	       ((eq? pathhead '@)
		(rec (stream-filter-map sxml-element-attributes l)))
	       ((symbol? pathhead)
		(rec (stream-fold-right
		      (lambda (v rest)
			(with-sxml-element/else
			 v
			 (lambda (name attrs body)
			   (if (eq? name pathhead)
			       (cons v rest)
			       rest))
			 (thunk rest)))
		      '()
		      lbodies)))
	       ((natural0? pathhead)
		;; XX don't error on overrun?
		(rec (list (stream-ref l pathhead))))
	       ((string? pathhead)
		;; compare and return a boolean instead of a list of values
		;; XX hm hacky?
		(let* ((bodies (stream->list bodies)))
		  (and (every string? bodies) ;; XX too narrow?
		       (string=? (strings-append bodies)
				 pathhead))))
	       ((pair? pathhead)
		;; filtering
		(rec (stream-filter
		      (lambda (v)
			;; XX relying on it returning a boolean
			;; (thankfully stream-filter is now
			;; restrictive), i.e. the hack above
			(_ssxpath-matches* pathhead
					   (list v)
					   (cons pathhead context)))
		      l)))
	       (else
		(error "invalid element in path:" pathhead)))))))

(define cj-ssxpath:nothing (box 'nothing))
;; avoiding gensym makes ccache work better!... (and box really
;; guarantees uniqueness; unlike list which might not, who knows)


(define-typed (_ssxpath-matches path #(sxml-element? element) context)
  (_ssxpath-matches* path (list element) context))

;; auto-currying:

(define (ssxpath-matches path
			 #!optional
			 (element cj-ssxpath:nothing)
			 (context '()))
  (if (eq? element cj-ssxpath:nothing)
      (lambda (element #!optional (context '()))
	(_ssxpath-matches path element context))
      (_ssxpath-matches path element context)))

(define (ssxpath-matches* path
			  #!optional
			  (elements cj-ssxpath:nothing)
			  (context '()))
  (if (eq? elements cj-ssxpath:nothing)
      (lambda (elements #!optional (context '()))
	(_ssxpath-matches* path elements context))
      (_ssxpath-matches* path elements context)))


;; and boolean-returning variants:

(define (ssxpath:->boolean v)
  (FV (v)
      (not (or (null? v)
	       (not v)))))

(define (ssxpath-matches? path
			 #!optional
			 (element cj-ssxpath:nothing)
			 (context '()))
  (if (eq? element cj-ssxpath:nothing)
      (lambda (element #!optional (context '()))
	(ssxpath:->boolean (_ssxpath-matches path element context)))
      (ssxpath:->boolean (_ssxpath-matches path element context))))

(define (ssxpath-matches*? path
			   #!optional
			   (elements cj-ssxpath:nothing)
			   (context '()))
  (if (eq? elements cj-ssxpath:nothing)
      (lambda (elements #!optional (context '()))
	(ssxpath:->boolean (_ssxpath-matches* path elements context)))
      (ssxpath:->boolean (_ssxpath-matches* path elements context))))




(TEST
 > (define sm (compose stream->list ssxpath-matches*))
 > (sm '() '((a (b))))
 ((a (b)))
 ;; auto-currying:
 > ((ssxpath-matches* '()) '((a (b))))
 ((a (b)))
 ;; boolean variants:
 > ((ssxpath-matches*? '()) '((a (b))))
 #t
 > ((ssxpath-matches*? '(a)) '((a (b))))
 #t
 > ((ssxpath-matches*? '(x)) '((a (b))))
 #f
 ;; pass single element instead of list:
 > (%try-error (sm '() '(a (b))))
 #(error "l does not match list-not-element?:" (a (b)))
 > (sm '(b) '((a (b))))
 ()
 > (sm '(a) '((a (b)) (d (e)) (a (c))))
 ((a (b)) (a (c)))
 ;; which is the same as either:
 > (stream->list (ssxpath-matches* '(a) '((a (b)) (d (e)) (a (c)))))
 ((a (b)) (a (c)))
 > (stream->list ((ssxpath-matches* '(a)) '((a (b)) (d (e)) (a (c)))))
 ((a (b)) (a (c)))
 ;; /same
 > (%try-error ((ssxpath-matches '(a)) '((a (b)) (d (e)) (a (c)))))
 #(error "element does not match sxml-element?:" ((a (b)) (d (e)) (a (c))))

 > (sm '(a /) '((a (b)) (d (e)) (a (c))))
 ((b) (c))
 > (sm '(a b /) '((a (b "world")) (a (b (c)))))
 ("world" (c))
 > (sm '(a * /) '((a (b "world")) (a (b (c "here")))))
 ("world" (c "here"))
 > (sm '(a * * /) '((a (b "world")) (a (b (c "here")))))
 ("here")
 > (sm '(body * a @ href /)
       '((body (p "hm " (a (@ (href "hah"))) "."))))
 ("hah")
 > (sm '(body p 0 a @ href /)
       '((body (p "hm " (a (@ (href "hah"))) ".")
	       (p "hm " (a (@ (href "hah2"))) "."))))
 ("hah")
 > (sm '(body * 1 a @ href /)
       '((body (p "hm " (a (@ (href "hah"))) ".")
	       (p "hm " (a (@ (href "hah2"))) "."))))
 ("hah2")
 ;; (interesting, even attribute bodies are lists. Of course, when
 ;; treating them that way..: )
 > (sm '(* @ * /) '((a (@ (b "hello") (c "world")))))
 ("hello" "world")
 > (sm '(p 0 /) '((p "hello" " world") (p "etc.")))
 ("hello" " world")

 ;; boolean hack feature --- huh overload of result type, how exactly?
 > (ssxpath-matches* '(p 0 "hello world") '((p "hello" " world") (p "etc.")))
 #t
 > (ssxpath-matches* '(p 1 "hello world") '((p "hello" " world") (p "etc.")))
 #f
 > (ssxpath-matches* '(p 1 "etc.") '((p "hello" " world") (p "etc.")))
 #t

 ;; and actually asking for booleans:
 > (ssxpath-matches*? '(p 0 "hello world") '((p "hello" " world") (p "etc.")))
 #t
 > (ssxpath-matches*? '(p 1 "hello world") '((p "hello" " world") (p "etc.")))
 #f
 
 ;; sub-match feature
 > (sm '(body * (a @ href "hah"))
       '((body (p "hm " (a (@ (href "hah"))) ".")
 	       (p "hm " (a (@ (href "hah2"))) "."))))
 ((p "hm " (a (@ (href "hah"))) "."))
 > (sm '(body * * (@ href "hah"))
       '((body (p "hm " (a (@ (href "hah"))) ".")
	       (p "hm " (a (@ (href "hah2"))) "."))))
 ((a (@ (href "hah"))))
 > (sm '(body * a (@ href "hah") /)
       '((body (p "hm "
		  (a (@ (href "hah")) "one")
		  (a (@ (href "hah2")) "two")
		  "."))))
 ("one")
 )

