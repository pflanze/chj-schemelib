;; this is a compatibility wrapper for chjmodule's TEST infrastructure
;; TODO: eliminate

(require define-macro-star
	 test
	 cj-exception)

(export TEST ;; re-export
	%try
	;; test-at-inittime
	;; define-tests
	;; no-test
	;; %exception-text
	;; if-TEST
	;; %values
	)

;; (global
;;  *cj-test:do-test*)



(define (cj-test:try thunk)
  (with-exception/continuation-catcher
   (lambda (e)
     (list 'exception text: (exception/continuation-text e)))
   (lambda ()
     (list 'value (thunk)))))

(define-macro* (%try expr)
  `(cj-test:try (lambda ()
                  ,expr)))

