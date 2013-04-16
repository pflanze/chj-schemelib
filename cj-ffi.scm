
(define-macro* (define-constant-from-C name)
  (assert* symbol? name
	   (lambda (name)
	     `(define ,name (##c-code ,(string-append "___RESULT= ___FIX("
						      (symbol->string name)
						      ");"))))))
