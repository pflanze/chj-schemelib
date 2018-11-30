;;; Copyright 2006-2008 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


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

