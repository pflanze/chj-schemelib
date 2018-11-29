;; Copyright 2011-2018 by Christian Jaeger <ch@christianjaeger.ch>


(require define-macro-star
	 (simple-match-1 match*)
	 (cj-source-util-2 assert))

(export (macro expansion)
	(macro macro-expand-all))


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

(define-macro* (macro-expand-all expr)
  `(expansion ,expr))

