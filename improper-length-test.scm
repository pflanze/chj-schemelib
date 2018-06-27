;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-source-util improper-length)
	 test)

(TEST
 > (improper-length '())
 0
 > (improper-length '(1))
 1
 > (improper-length '(a b c))
 3
 > (improper-length '(a b . c))
 -3
 > (improper-length '(a . c))
 -2
 > (improper-length 'c)
 -1)

