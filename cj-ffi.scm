
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

(define-macro* (define-macro-symbol-replace-_-with nam cvar)
  (with-gensyms
   (CVAR V)
   `(begin
      (define ,CVAR ,cvar)
      (define-macro (,nam ,V)
	,(list 'quasiquote
	       `((symbol-replace-_-with/ ,CVAR)
		 ',(list 'unquote V)))))))

(TEST
 > ((lambda (foo)
      (define-macro-symbol-replace-_-with R foo)
      (define somethingwhateverelse #f)
      (R blu_))
    #\x)
 blux
 )


(define symbol->string*
  (compose symbol->string source-code))

