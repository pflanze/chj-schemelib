;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


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

(define (ssxpath-match path l #!optional (context '()))
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
		     (ssxpath-match path*
				    l*
				    (cons pathhead context)))))
	 (cond ((eq? pathhead '/)
		(rec bodies))
	       ((eq? pathhead '*)
		(rec (stream-filter sxml-element? lbodies)))
	       ((eq? pathhead '@)
		(rec (stream-map sxml-element-attributes l)))
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
		       (equal? (apply string-append bodies)
			       pathhead))))
	       ((pair? pathhead)
		;; filtering
		(rec (stream-filter
		      (lambda (v)
			(let ((res (ssxpath-match pathhead
						  (list v)
						  (cons pathhead context))))
			  (assert (boolean? res))
			  res))
		      l)))
	       (else
		(error "invalid element in path:" pathhead)))))))

(TEST
 > (define sm (compose stream->list ssxpath-match))
 > (sm '() '((a (b))))
 ((a (b)))
 > (sm '(b) '((a (b))))
 ()
 > (sm '(a) '((a (b)) (d (e)) (a (c))))
 ((a (b)) (a (c)))
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

 ;; boolean hack feature
 > (ssxpath-match '(p 0 "hello world") '((p "hello" " world") (p "etc.")))
 #t
 > (ssxpath-match '(p 1 "hello world") '((p "hello" " world") (p "etc.")))
 #f
 > (ssxpath-match '(p 1 "etc.") '((p "hello" " world") (p "etc.")))
 #t
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

(define (ssxpath path)
  (lambda (v)
    (ssxpath-match path v)))

