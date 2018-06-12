;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-symbol)

(export (macro fixnum-natural?)
	(macro fixnum-natural0?))


(define-macro* (fixnum-natural? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe))
		       (##and (##fixnum? ,x)
			      (##< 0 ,x)))))

(define-macro* (fixnum-natural0? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe))
		       (##and (##fixnum? ,x)
			      (##<= 0 ,x)))))

