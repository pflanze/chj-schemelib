;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         (cj-gambit-sys max-fixnum min-fixnum)
	 cj-symbol
         test)

(export (macros fixnum-natural?
                fixnum-natural0?
                incrementable-fixnum?
                decrementable-fixnum?))


(define-macro* (fixnum-natural? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe) (standard-bindings)
				  (extended-bindings) (block))
		       (##namespace ("" and fixnum? <= <))
		       (and (fixnum? ,x)
			    (< 0 ,x)))))

(define-macro* (fixnum-natural0? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe) (standard-bindings)
				  (extended-bindings) (block))
		       (##namespace ("" and fixnum? <= <))
		       (and (fixnum? ,x)
			    (<= 0 ,x)))))

(define-macro* (incrementable-fixnum? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe) (standard-bindings)
				  (extended-bindings) (block))
		       (##namespace ("" and fixnum? <= <))
		       (and (fixnum? ,x)
			    (< ,x max-fixnum)))))

(define-macro* (decrementable-fixnum? e)
  (with-gensym x
	       `(##let ((,x ,e))
		       (##declare (fixnum) (not safe) (standard-bindings)
				  (extended-bindings) (block))
		       (##namespace ("" and fixnum? <= <))
		       (and (fixnum? ,x)
			    (> ,x min-fixnum)))))


(TEST
 > (incrementable-fixnum? 0)
 #t
 > (incrementable-fixnum? 0.)
 #f
 > (incrementable-fixnum? -1000000)
 #t
 > (incrementable-fixnum? min-fixnum)
 #t
 > (incrementable-fixnum? max-fixnum)
 #f
 > (incrementable-fixnum? (dec max-fixnum))
 #t

 > (decrementable-fixnum? max-fixnum)
 #t
 > (decrementable-fixnum? min-fixnum)
 #f
 > (decrementable-fixnum? (inc min-fixnum))
 #t)
