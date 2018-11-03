;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 jclass
	 (more-oo let.-static)
	 list-util
	 stream
	 cj-sxml
	 cj-env	;; natural0?, should be moved
	 (oo-lib-string strings-append)
	 debuggable-promise
	 predicates
	 (oo-util-lazy iseq-of)
	 test
	 )


(export ssxpath-matches ;; on single elements; auto-curring
	ssxpath-matches* ;; on lists of elements; auto-curring
	)


(possibly-use-debuggable-promise)

;; XXX move
;; seq is a bit of a mis-leading term, though. what about vectors etc.?
;; sgh linked ? ilseq  well. iseq not seq  ok?
;;/move



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


;; (Only loosely related:
;;   https://en.wikipedia.org/wiki/Xpath
;;   http://wiki.call-cc.org/eggref/4/sxpath
;;   https://www.gnu.org/software/guile/manual/html_node/SXPath.html )



;; hacky? keep in sync with dispatching cond
(def ssxpath-path-item?
     (either symbol? ;; incl. / * @
	     exact-natural0?
	     string?
	     pair?))

(jclass (ssxpath-match #(sxml? value) ;; not just |sxml-element?|
		       #((maybe natural0?) maybe-index)
		       ;; ^ #f for the "entry match" when matching a
		       ;; single element (via ssxpath-matches, not
		       ;; ssxpath-matches*).
		       #((iseq-of ssxpath-match?) rest)
		       ;; ^ part of the list of matches following this
		       ;; one in its match layer (i.e. ssxpath-match
		       ;; is forming an intrusive list; we wouldn't
		       ;; need the cons cells around except for using
		       ;; standard functions)
		       #((maybe ssxpath-path-item?) pathhead)
		       ;; ^ #f for the first entry (initial list)
		       #((maybe ssxpath-match?) maybe-parent)
		       ;; ^ ditto
		       )

	;; wait wrong orderanyway?
	(def-method (path s)
	  (if pathhead
	      (cons pathhead (if maybe-parent
				 (ssxpath-match.path maybe-parent)
				 '()))
	      '()))

	;; a path of only using indexing. XX BUT, item indices versus
	;; element indices? !
	(def-method- (index-path s)
	  (let lp ((m s)
		   (p '()))
	    (let.-static (ssxpath-match. (maybe-index maybe-parent) m)
			 (let ((p* (cons maybe-index p)))
			   (if maybe-parent
			       (lp maybe-parent
				   p*)
			       p*)))))

	;;XX still figuring things out. should probably be called |path|.
	(def-method (precise-path s)
	  (let lp ((m s)
		   (p '()))
	    (let.-static (ssxpath-match. (maybe-index pathhead maybe-parent) m)
			 (let ((p* (cons* pathhead maybe-index p)))
			   (if maybe-parent
			       (lp maybe-parent
				   p*)
			       p*)))))
	

	;;X? what was that ...?
	(def-method (individual-path s)
	  (if pathhead
	      (cons* maybe-index pathhead
		     (if maybe-parent
			 (ssxpath-match.individual-path maybe-parent)
			 '()))
	      '())))



(def iseq-of-ssxpath-match? (iseq-of ssxpath-match?))


(def (__ssxpath-matches*/context path
				 #(iseq-of-ssxpath-match? ms)
				 #(boolean? first-call?)
				 #(boolean? want-ctx?))
     -> iseq-of-ssxpath-match?

     (if (null? path)
	 ms
	 (let-pair
	  ((pathhead path*) path)

	  (let* ((bodies
		  ;; all of the values / the {current layer of
		  ;; element}'s bodies appended
		  (stream-fold-right
		   (lambda (m res)
		     ;; for each body element, add a new match context
		     ;; frame
		     (let ((v (ssxpath-match.value m)))
		       (if (sxml-element? v)
			   (stream-fold-right/iota
			    (lambda (v tail i)
			      (cons (ssxpath-match v i tail '/ m)
				    tail))
			    res
			    (sxml-element-body v))
			   (cons m res))))
		   '()
		   ms))

		 (lbodies (if first-call?
			      ms
			      bodies))
		 (rec (lambda (ms*)
			(__ssxpath-matches*/context path*
						    ms*
						    #f
						    want-ctx?))))
	    (cond ((eq? pathhead '/)
		   ;; use the current layer of element's bodies;
		   ;; XX wrap with "/" *now* instead of already having
		   ;;    been done above?
		   (rec bodies))

		  ((eq? pathhead '//)
		   (error "// is not implemented (yet)"))

		  ((eq? pathhead '*)
		   ;; pick all elements (filter out non-elements)
		   (rec (stream-fold-right/iota+rest
			 (lambda (m res i rest)
			   (let ((v (ssxpath-match.value m)))
			     (if (sxml-element? v)
				 (cons (ssxpath-match v
						      i
						      res
						      pathhead
						      m)
				       res)
				 res)))
			 '()
			 lbodies)))

		  ((eq? pathhead '@)
		   ;; pick attributes
		   (rec (stream-fold-right/iota+rest
			 (lambda (m res i rest)
			   (let ((v (ssxpath-match.value m)))
			     (if (sxml-element? v)
				 (cond ((sxml-element-maybe-attributes v)
					=> (lambda (atts)
					     (cons (ssxpath-match atts
								  i
								  res
								  pathhead
								  m)
						   res)))
				       (else
					res))
				 res)))
			 '()
			 ms)))

		  ((symbol? pathhead)
		   ;; pick elements with that name
		   (rec (stream-fold-right/iota+rest
			 (lambda (m res i rest)
			   (with-sxml-element/else
			    (ssxpath-match.value m)
			    (lambda (name attrs body)
			      (if (eq? name pathhead)
				  (cons (ssxpath-match
					 (ssxpath-match.value m)
					 i
					 res
					 pathhead
					 m)
					res)
				  res))
			    (& res)))
			 '()
			 lbodies)))

		  ((exact-natural0? pathhead)
		   (Maybe:if (stream-Maybe-ref ms pathhead)
			     (rec (list (ssxpath-match
					 (ssxpath-match.value it)
					 pathhead
					 (ssxpath-match.rest it) ;; XXX?
					 pathhead
					 it)))
			     '()))

		  ((string? pathhead)
		   (let* ((bodies (map ssxpath-match.value
				       (stream->list bodies))))
		     (if (and (every string? bodies) ;; XX too narrow?
			      (string=? (strings-append bodies)
					pathhead))
			 (begin
			   ;;(step)
			   ;; XX is it OK to return the element,
			   ;; versus its body here?
			   ms)
			 (begin
			   ;;(step)
			   '()))))

		  ((pair? pathhead)
		   ;; filtering
		   (rec (stream-filter
			 (lambda (v)
			   (pair?
			    (force
			     (__ssxpath-matches*/context
			      pathhead
			      (list v)
			      #f
			      #f))))
			 ms)))

		  (else
		   (error "invalid element in path:" pathhead)))))))

(def cj-ssxpath:nothing (box 'nothing))
;; avoiding gensym makes ccache work better!... (and box really
;; guarantees uniqueness; unlike list which might not)


(def (iseq-not-element? v)
     (FV (v)
	 (if (pair? v)
	     (not (symbol? (force (car v))))
	     (null? v))))


(def (cj-ssxpath:prep-elements elements)
     (.fold-right/iota
      elements
      (lambda (e res i)
	(cons (ssxpath-match e i res #f #f) res))
      '()))


(def (_ssxpath-matches*/context path #(iseq-not-element? elements))
     (__ssxpath-matches*/context path
				 (cj-ssxpath:prep-elements elements)
				 #t
				 #t))

(def (_ssxpath-matches/context path #(sxml-element? element))
     (__ssxpath-matches*/context path
				 (list (ssxpath-match element #f '() #f #f))
				 #t
				 #t))

(def (_ssxpath-matches* path #(iseq-not-element? elements))
     (stream-map ssxpath-match.value
		 (__ssxpath-matches*/context
		  path
		  (cj-ssxpath:prep-elements elements)
		  #t
		  ;; XX change to #f and
		  ;; implement it then
		  ;; remove stream-map:
		  #t)))

(def (_ssxpath-matches path #(sxml-element? element))
     (stream-map ssxpath-match.value
		 (__ssxpath-matches*/context
		  path
		  (list (ssxpath-match element #f '() #f #f))
		  #t
		  ;; XX ditto
		  #t)))

;; auto-currying:

(def (ssxpath-matches path
		      #!optional
		      (element cj-ssxpath:nothing))
     (def cont (C _ssxpath-matches path _))
     (if (eq? element cj-ssxpath:nothing) cont (cont element)))

(def (ssxpath-matches* path
		       #!optional
		       (elements cj-ssxpath:nothing))
     (def cont (C _ssxpath-matches* path _))
     (if (eq? elements cj-ssxpath:nothing) cont (cont elements)))

;; with context:

(def (ssxpath-matches/context path
			      #!optional
			      (element cj-ssxpath:nothing))
     (def cont (C _ssxpath-matches/context path _))
     (if (eq? element cj-ssxpath:nothing) cont (cont element)))

(def (ssxpath-matches*/context path
			       #!optional
			       (elements cj-ssxpath:nothing))
     (def cont (C _ssxpath-matches*/context path _))
     (if (eq? elements cj-ssxpath:nothing) cont (cont elements)))




;; and boolean-returning variants:

(def (ssxpath:->boolean v)
     (FV (v)
	 (not (or (null? v)
		  (not v)))))

;; (can't use compose-function here because of the currying)

(def (ssxpath-matches? path
		       #!optional
		       (element cj-ssxpath:nothing))
     (def cont
	  (lambda (element)
	    (ssxpath:->boolean (_ssxpath-matches/context path element))))
     (if (eq? element cj-ssxpath:nothing) cont (cont element)))

(def (ssxpath-matches*? path
			#!optional
			(elements cj-ssxpath:nothing))
     (def cont
	  (lambda (elements)
	    (ssxpath:->boolean (_ssxpath-matches*/context path elements))))
     (if (eq? elements cj-ssxpath:nothing) cont (cont elements)))



(TEST
 > (def sm* (comp-function stream->list ssxpath-matches*))
 > (def sm (comp-function stream->list ssxpath-matches))
 > (def sm*/c (comp-function stream->list ssxpath-matches*/context))
 > (def sm/c (comp-function stream->list ssxpath-matches/context))

 > (sm* '() '((a (b))))
 ((a (b)))
 ;; auto-currying:
 > (.list ((ssxpath-matches* '()) '((a (b)))))
 ((a (b)))
 > (.list ((ssxpath-matches* '(a b)) '((a (b)))))
 ((b))
 > (.list ((ssxpath-matches '(a b)) '(a (b))))
 ((b))
 ;; boolean variants:
 > ((ssxpath-matches*? '()) '((a (b))))
 #t
 > ((ssxpath-matches*? '(a)) '((a (b))))
 #t
 > ((ssxpath-matches*? '(x)) '((a (b))))
 #f
 ;; pass single element instead of list:
 > (%try-error (sm* '() '(a (b))))
 #(error "elements does not match iseq-not-element?:" (a (b)))
 > (sm* '(b) '((a (b))))
 ()
 > (sm* '(a) '((a (b)) (d (e)) (a (c))))
 ((a (b)) (a (c)))
 ;; which is the same as:
 > (.list ((ssxpath-matches* '(a)) '((a (b)) (d (e)) (a (c)))))
 ((a (b)) (a (c)))

 > (%try-error ((ssxpath-matches '(a)) '((a (b)) (d (e)) (a (c)))))
 #(error "element does not match sxml-element?:" ((a (b)) (d (e)) (a (c))))

 > (sm* '(*) '((a (b "world")) (a (b (c "here")))))
 ;; (Matches return the full elements that match the last item in the
 ;; path, not just their bodies.)
 ((a (b "world")) (a (b (c "here"))))
 > (sm* '(a b) '((a (b "world")) (a (b (c "here")))))
 ((b "world") (b (c "here")))
 > (sm* '(a *) '((a (b "world")) (a (b (c "here")))))
 ((b "world") (b (c "here")))

 > (sm* '(a /) '((a (b)) (d (e)) (a (c))))
 ((b) (c))
 > (sm* '(a b /) '((a (b "world")) (a (b (c)))))
 ("world" (c))
 > (sm* '(a * /) '((a (b "world")) (a (b (c "here")))))
 ("world" (c "here"))

 > (sm* '(a * * /) '((a (b "world")) (a (b (c "here")))))
 ("here")
 > (sm* '(body * a @ href /)
	'((body (p "hm " (a (@ (href "hah"))) "."))))
 ("hah")
 > (sm* '(body p 0 a @ href /)
	'((body (p "hm " (a (@ (href "hah"))) ".")
		(p "hm " (a (@ (href "hah2"))) "."))))
 ("hah")
 > (sm* '(body * 1 a @ href /)
	'((body (p "hm " (a (@ (href "hah"))) ".")
		(p "hm " (a (@ (href "hah2"))) "."))))
 ("hah2")
 ;; (interesting, even attribute bodies are lists. Of course, when
 ;; treating them that way..: )
 > (sm* '(* @ * /) '((a (@ (b "hello") (c "world")))))
 ("hello" "world")
 > (sm* '(p 0 /) '((p "hello" " world") (p "etc.")))
 ("hello" " world")

 > (.list (ssxpath-matches* '(p 0) '((p "hello" " world") (p "etc."))))
 ((p "hello" " world"))
 > (.list (ssxpath-matches* '(p 1) '((p "hello" " world") (p "etc."))))
 ((p "etc."))
 > (.list (ssxpath-matches* '(p 2) '((p "hello" " world") (p "etc."))))
 ()
 > (.list (ssxpath-matches* '(p 0 "hello world")
			    '((p "hello" " world") (p "etc."))))
 ((p "hello" " world"))
 ;; ^ XX is it OK to return the element, versus its body here? ?
 > (.list (ssxpath-matches* '(p 1 "hello world")
			    '((p "hello" " world") (p "etc."))))
 ()
 > (.list (ssxpath-matches* '(p 1 "etc.")
			    '((p "hello" " world") (p "etc."))))
 ((p "etc.")) ;; ditto

 ;; and, asking for booleans:
 > (ssxpath-matches*? '(p 0 "hello world") '((p "hello" " world") (p "etc.")))
 #t
 > (ssxpath-matches*? '(p 1 "hello world") '((p "hello" " world") (p "etc.")))
 #f
 
 ;; sub-match feature
 > (sm* '(body * (a @ href "hah"))
	'((body (p "hm " (a (@ (href "hah"))) ".")
		(p "hm " (a (@ (href "hah2"))) "."))))
 ((p "hm " (a (@ (href "hah"))) "."))
 > (sm* '(body * * (@ href "hah"))
	'((body (p "hm " (a (@ (href "hah"))) ".")
		(p "hm " (a (@ (href "hah2"))) "."))))
 ((a (@ (href "hah"))))
 > (sm* '(body * a (@ href "hah") /)
	'((body (p "hm "
		   (a (@ (href "hah")) "one")
		   (a (@ (href "hah2")) "two")
		   "."))))
 ("one")


 ;; more indexing testing
 ;; > (.list (ssxpath-matches*/context '(0) '((p "hello" " world") (p "etc."))))
 ;; (#((ssxpath-match)
 ;;    (p "hello" " world")
 ;;    0
 ;;    ((p "etc."))
 ;;    0
 ;;    #((ssxpath-match) (p "hello" " world") 0 ((p "etc.")) #f #f)))
 ;; > (.list (ssxpath-matches*/context '(1) '((p "hello" " world") (p "etc."))))
 ;; (#((ssxpath-match) (p "etc.") 1 () 1 #((ssxpath-match) (p "etc.") 1 () #f #f)))


 > (def l '((p (@ (class "a")) "hello" " world")
	    (p "hello" " world")
	    (table (tr (td "foo")))
	    (p (@ (class "b")) "etc.")))

 > (def doc `(body ,@l))

 ;; properly understanding * vs. / :
 > (sm '(* table) doc)
 ((table (tr (td "foo"))))
 > (sm '(* / table) doc)
 ()
 > (sm '(/ table) doc)
 ()
 > (sm '(/ tr) doc)
 ((tr (td "foo")))
 > (sm '(* / tr) doc)
 ((tr (td "foo")))
 ;; '* can't just be added arbitrarily, either, it determines the
 ;; requested depth where to find the target:
 > (sm '(* * table) doc)
 ()
 > (sm '(* * tr) doc)
 ((tr (td "foo")))
 ;; continuing to drill down 'manually':
 > (sm* '(* td) #)
 ((td "foo"))

 ;; / on non-elements:
 > (sm* '(/) '((foo "bar") "foo"))
 ("bar" "foo") ;; Good or bad idea?
 ;; versus filtering for elements first:
 > (sm* '(* /) '((foo "bar") "foo"))
 ("bar")
 > (sm* '(*) '((foo "bar") "foo"))
 ((foo "bar"))

 ;; @ on non-elements:
 > (sm* '(@) '((foo (@ (a "1")) "bar") (bar "bar") "foo"))
 ((@ (a "1")))

 
 > (map .maybe-index (sm/c '(* table) doc))
 (2)
 > (map .index-path (sm/c '(* table) doc))
 ((#f ;; entry has no index
   0  ;; XXX strip, right?
   2  ;; table is the 2nd element of the '*' result
   2  ;; XXX strip, right?
   ))

 ;; ;; positions:
 ;; > (.map (sm*/c '(p @) l) .maybe-index)
 ;; (0 3) ;; XX why does it give (0 2) ??
 ;; > (.map (sm*/c '(p "hello world") l) .maybe-index)
 ;; (0 1)
 ;; ;; index-path
 ;; > (.map (sm*/c '(p @) l) .index-path)
 ;; ((0 0) (3 0)) ;; XX why ((0 0 0) (2 3 3))  *?*?*  ah 2  second p  sigh!  *?*
 ;; > (.map (sm*/c '(p "hello world") l) .index-path)
 ;; ((0 1) (1 0)) ;; XX?  body being 1, odd thing? *?*

 ;; ;; ssxpath-match.path feature (should get back the original path;
 ;; ;; always?? then pointless, perhaps not always):
 ;; > (.map (sm*/c '(1) l) .path)
 ;; ((1))
 ;; > (.map (sm*/c '(p) l) .path)
 ;; ((p) (p) (p))
 ;; > (.map (sm*/c '(*) l) .path)
 ;; ((*) (*) (*) (*))
 ;; ;; but, individual-path makes it interesting:
 ;; > (.map (sm*/c '(1) l) .path)
 ;; ((1))
 ;; > (.map (sm*/c '(p) l) .path)
 ;; ((p 0) (p 1) (p 3))
 ;; > (.map (sm*/c '(*) l) .path)
 ;; ((* 0) (* 1) (* 2) (* 3))

 
 )
