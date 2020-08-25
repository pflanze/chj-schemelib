;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require optim-values
	 test)


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#%call-with-values (lambda () (values 3 4 5)) (lambda (a b c) b))
 (##let ((GEN:V-3073 ((lambda () (values 3 4 5)))))
	(##if (##let ()
		     (##declare
                      (block)
                      (standard-bindings)
                      (extended-bindings)
                      (not safe)
                      (fixnum))
		     (##namespace ("" and =))
		     (and (##values? GEN:V-3073)
			  (= (@values-length GEN:V-3073) 3)))
	      ((lambda (a b c) b)
	       (@values-ref GEN:V-3073 0)
	       (@values-ref GEN:V-3073 1)
	       (@values-ref GEN:V-3073 2))
	      (optim-values:error GEN:V-3073 3)))
 > (%call-with-values (lambda () (values 3 4 5)) (lambda (a b c) b))
 4
 > (expansion#%call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 (call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 > (%call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 4)

