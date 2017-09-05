(require (define-macro-star)
	 (simple-match))


(define-macro* (define-strict-and-lazy
		 strict-name
		 stream-name
		 #!key
		 (aliases '()) ;; list of `(name strict-name stream-name)
		 #!rest
		 exprs)
  ;; replaces |DELAY| with delay or 'nothing', same for |FV| and
  ;; |FORCE|, and binds each name in aliases to strict-name or
  ;; stream-name
  `(begin
     (define ,strict-name
       (let ,(map (lambda (alias-lis3)
		    (let ((l (source-code alias-lis3)))
		      `(,(car l) ,(cadr l))))
		  (source-code aliases))
	 (##define-syntax DELAY
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((DELAY expr)
			       expr))
			     stx)))
	 (##define-syntax FV
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((FV vars . body)
			       `(begin
				  ,@body)))
			     stx)))
	 (##define-syntax FORCE
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((FORCE expr)
			       expr))
			     stx)))
	 ,@exprs))
     (define ,stream-name
       (let ,(map (lambda (alias-lis3)
		    (let ((l (source-code alias-lis3)))
		      `(,(car l) ,(caddr l))))
		  (source-code aliases))
	 (##define-syntax DELAY
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((DELAY expr)
			       `(delay ,expr)))
			     stx)))
	 (##define-syntax FV
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((FV vars . body)
			       `(let ,(map (lambda (v)
					     `(,v (force ,v)))
					   (source-code vars))
				  ,@body)))
			     stx)))
	 (##define-syntax FORCE
			  (lambda (stx)
			    (cj-sourcify-deep
			     (match*
			      stx
			      ((FORCE expr)
			       `(force ,expr)))
			     stx)))
	 ,@exprs))))
