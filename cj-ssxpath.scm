;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Simple XPath alike matching for SXML.

;; "./foo/bar[0]" == '(foo bar 0)
;; "./foo/bar[0]/baz" == '(foo bar 0 baz)
;; "./foo/*/[0]/baz" == '(foo * 0 baz)

;; also, @ for attribute matches, (* @ *) returns all attribute values
;; of all tags in the current context.

;; NOTE: returns the *appended bodies* of the elements (or their
;; attributes) that match the complete path, not the whole elements
;; (and drops the attributes unless in intermediate steps when they
;; are needed in the next matching level).

(define (ssxpath-match path v)
  (let* ((l (if (or (null? v)
		    (and (pair? v)
			 (not (sxml-element? v))))
		v
		(list v))))
    (if (null? path)
	l
	(let-pair
	 ((pathhead path*) path)

	 (let ((perhaps-cons (if (and (pair? path*)
				      (eq? (car path*) '@))
				 cons
				 (lambda (a b) b))))
	   (cond ((eq? pathhead '*)
		  ;; *almost* same as handling of (symbol?
		  ;; pathhead), just not testing for element
		  ;; name equivalence
		  (ssxpath-match
		   path*
		   (fold-right
		    (lambda (v rest)
		      (with-sxml-element/else
		       v
		       (lambda (name attrs body)
			 (append (perhaps-cons attrs body)
				 rest))
		       (thunk rest)))
		    '()
		    l)))
		 ((symbol? pathhead)
		  (ssxpath-match
		   path*
		   (fold-right
		    (lambda (v rest)
		      (with-sxml-element/else
		       v
		       (lambda (name attrs body)
			 (if (eq? name pathhead)
			     (append (perhaps-cons attrs body)
				     rest)
			     rest))
		       (thunk rest)))
		    '()
		    l)))
		 ((natural0? pathhead)
		  (ssxpath-match path*
				 ;; XX don't error on overrun?
				 (sxml-element-body (list-ref l pathhead))))
		 (else
		  (error "invalid element in path:" pathhead))))))))

(TEST
 > (ssxpath-match '() '(a (b)))
 ((a (b)))
 > (ssxpath-match '(b) '(a (b)))
 ()
 > (ssxpath-match '(a) '(a (b)))
 ((b))
 > (ssxpath-match '(a b) '((a (b "world")) (a (b (c)))))
 ("world" (c))
 > (ssxpath-match '(a * *) '((a (b "world")) (a (b (c "here")))))
 ("here")
 > (ssxpath-match '(body * a @ href) '(body (p "hm " (a (@ (href "hah"))) ".")))
 ("hah")
 > (ssxpath-match '(body 0 a @ href) '(body (p "hm " (a (@ (href "hah"))) ".")
					    (p "hm " (a (@ (href "hah2"))) ".")))
 ("hah")
 > (ssxpath-match '(body 1 a @ href) '(body (p "hm " (a (@ (href "hah"))) ".")
					    (p "hm " (a (@ (href "hah2"))) ".")))
 ("hah2")
 ;; (interesting, even attribute bodies are lists. Of course, when
 ;; treating them that way..: )
 > (ssxpath-match '(* @ *) '(a (@ (b "hello") (c "world"))))
 ("hello" "world")
 )

(define (ssxpath path)
  (lambda (v)
    (ssxpath-match path v)))

