
(define-macro* (expansion expr . exprs)
  (define CONT 'cont) ;; |...| looks nice, but may be confusing
  (let ((expr (if (pair? exprs)
		  (sourcify (cons expr exprs) stx)
		  expr)))
    (let ((code
	   (##decompile
	    (eval `(lambda ()
		     ,expr
		     ,CONT)))))
      (list
       'quote
       (match*
	code
	((LAMBDA _ _letrec)
	 _letrec)
	((LAMBDA _ form end)
	 (assert (equal? end CONT))
	 form))))))

