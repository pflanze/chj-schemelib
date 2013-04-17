
(define-macro* (define-constant-from-C name)
  (assert* symbol? name
	   (lambda (name)
	     `(define ,name (##c-code ,(string-append "___RESULT= ___FIX("
						      (symbol->string name)
						      ");"))))))

;; helper function to replace _ in identifiers:
(define (symbol-replace-_-with/ c)
  (assert* char? c
	   (lambda (c)
	     (compose* string->symbol
		       list->string
		       (cut map (lambda_
				 (if (eq? _ '#\_)
				     c
				     _))
			    <>)
		       string->list
		       symbol->string))))

(TEST
 > ((symbol-replace-_-with/ #\c) 'M_-bar)
 Mc-bar
 )

