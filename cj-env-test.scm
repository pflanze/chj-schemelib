;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; tests for cj-env, to break dependency cycle on test-lib

(require define-macro-star
	 cj-phasing
	 cj-inline-1
	 test
	 (test-lib %try-error)
	 cj-env-1)


(TEST
 > (define t (make-table))
 > (%try-error (table-update! t 'a inc))
 #(error "key not found")
 > (table-set! t 'a 1)
 > (table-update! t 'a inc)
 > (table-ref t 'a)
 2
 > (table-update! t 'b inc (lambda () 10))
 > (table-ref t 'b)
 10)

