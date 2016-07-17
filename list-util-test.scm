;;; Copyright 2010-2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 (test-lib %try-error)
	 srfi-1
	 (cj-env-1 dec inc identity)
	 (string-util-1 string-split)
	 (improper-list improper-length)
	 )

(TEST
 > (%try-error
    (trif-one '(a . b) identity (cut error "too many:" <>) (cut error "none")))
 #(error "too many:" (a . b))
 > (%try-error
    (trif-one '(a) identity (cut error "too many:" <>) (cut error "none")))
 a
 > (%try-error
    (trif-one '() identity (cut error "too many:" <>) (cut error "none")))
 #(error "none")
 )

