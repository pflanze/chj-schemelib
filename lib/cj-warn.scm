;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(define (warn msg . objs)
  (let ((port (current-error-port))
	(separator " "))
    (display msg port)
    (let lp ((objs objs))
      (cond ((null? objs)
	     (newline port))
	    ((pair? objs)
	     (display separator port)
	     (display (object->string (car objs)) port)
	     (lp (cdr objs)))
	    (else (error "improper list:" objs))))))
