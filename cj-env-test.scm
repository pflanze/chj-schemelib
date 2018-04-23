;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; tests for cj-env, to break dependency cycle on test-random

(require define-macro-star
	 cj-phasing
	 cj-inline-1
	 test
	 (test-lib-1 %try-error)
	 cj-source ;; cj-env-1 included in cj-source, sigh
	 )


(TEST
 > (define t (make-table))
 > (%try-error (table-update! t 'a inc-function))
 #(error "key not found")
 > (table-set! t 'a 1)
 > (table-update! t 'a inc-function)
 > (table-ref t 'a)
 2
 > (table-update! t 'b inc-function (lambda () 10))
 > (table-ref t 'b)
 10)

(TEST
 > (list-join '() 'a)
 ()
 > (list-join '(1 2) 'a)
 (1 a 2)
 > (list-join '(1) 'a 'rest)
 (1 . rest)
 > (list-join '(1 2) 'a 'rest)
 (1 a 2 . rest)
 > (list-join '() 'a 'rest)
 rest)
