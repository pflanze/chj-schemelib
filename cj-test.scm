;; this is a compatibility wrapper for chjmodule's TEST infrastructure

(require define-macro-star
	 test)

(define (cj-test:try thunk)
  (with-exception/continuation-catcher
   (lambda (e)
     (list 'exception text: (exception/continuation-text e)))
   (lambda ()
     (list 'value (thunk)))))

(define-macro* (%try expr)
  `(cj-test:try (lambda ()
                  ,expr)))

