(require (define-macro-star)
	 (simple-match))


(define-macro* (define-strict-and-lazy
		 strict-name
		 stream-name
		 expr)
  ;; replaces |DELAY| with delay or 'nothing', same for |FV| and |FORCE|
  `(begin
     (define ,strict-name
       (let ()
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
	,expr))
     (define ,stream-name
       (let ()
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
	,expr))))
