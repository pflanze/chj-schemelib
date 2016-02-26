(require define-macro-star
	 cj-match
	 cj-symbol
	 test)

(define-macro* (define-constants-from-C . names)
  `(begin
     ,@(map (lambda (name)
	      `(define-constant-from-C ,name))
	    names)))

;; helper function to replace _ in identifiers:
(define (symbol-replace-_-with/ c)
  (let ((cont (lambda (chars)
		(compose* string->symbol
			  list->string
			  (cut fold-right
			       (lambda (c res)
				 (if (eq? c '#\_)
				     (append chars res)
				     (cons c res)))
			       '()
			       <>)
			  string->list
			  symbol->string)))
	(c* (source-code c)))
    (mcase c
	   (char?
	    (cont (list c*)))
	   (string?
	    (cont (string->list c*)))
	   (symbol?
	    (cont (string->list (symbol->string c*)))))))

(TEST
 > ((symbol-replace-_-with/ #\c) 'M_-bar)
 Mc-bar
 > ((symbol-replace-_-with/ "abc") 'M_-bar)
 Mabc-bar
 > ((symbol-replace-_-with/ 'xyz) 'M_-bar)
 Mxyz-bar
 > ((symbol-replace-_-with/ "") 'M_-bar)
 M-bar
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

(define string*
  (compose string source-code))

