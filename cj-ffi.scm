
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

(define-macro* (define-symbol-replace-_-with nam cvar)
  ;; XX hm dangerously handling cvar: assumes that cvar isn't shadowed
  ;; between use of define-symbol-replace-_-with and its effected
  ;; definition.
  (with-gensym V
	       `(define-macro (,nam ,V)
		  ,(list 'quasiquote
			 `((symbol-replace-_-with/ ,(list 'unquote cvar))
			   ',(list 'unquote V))))))

(TEST
 > (define-symbol-replace-_-with R foo)
 > (define foo #\x)
 > (R blu_)
 blux
 )
