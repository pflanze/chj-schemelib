;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-1
	 test)

(def _cj-seen:nothing (gensym))

(def (make-seen?! . args)
     (let ((t (apply make-table args)))
       (lambda (val)
	 (let ((v (table-ref t val _cj-seen:nothing)))
	   (if (eq? v _cj-seen:nothing)
	       (begin
		 (table-set! t val #t)
		 #f)
	       #t)))))

(TEST
 > (def s?! (make-seen?!))
 > (s?! 3)
 #f
 > (s?! 3)
 #t
 > (s?! 3)
 #t
 > (s?! 4)
 #f
 > (s?! 4)
 #t
 > (def s2 (make-seen?!))
 > (s2 "foo")
 #f
 > (s2 "foo")
 #t
 > (s2 3)
 #f)


;; bah, ~copy-paste
(def (make-seen?+! . args)
     (let ((t (apply make-table args)))
       (values
	;; seen?
	(lambda (val)
	  ;; don't actually need _cj-seen:nothing, huh
	  (table-ref t val #f))
	;; seen!
	(lambda (val)
	  (table-set! t val #t)))))

