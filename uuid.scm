;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (predicates string-of-length)
	 (char-util char-alphanumeric?)
	 ;; and side effects:
	 (realrandom make-realrandom-alphanumeric-string-stream)
	 atomic-box)

(export random-uuid
	uuid?)


(def random-uuid-length 27)

(def uuid?
     (both (string-of-length random-uuid-length)
	   (string-of char-alphanumeric?)))

(def random-uuid
     (let ((*s (atomic-box (make-realrandom-alphanumeric-string-stream
			    random-uuid-length))))
       (lambda ()
	 (atomic-box.update!
	  *s
	  (lambda (s)
	    (let-pair ((a s*) (force s))
		      (values s* (-> uuid? a))))))))


