;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy cj-alist)


(define (sequencialpairs->pairs lis key-type? value-type?)
  (let rec ((lis lis))
    (if (null? lis)
	'()
	(let ((key (car lis))
	      (val (cadr lis)))
	  (assert (key-type? key))
	  (assert (value-type? val))
	  (cons (cons key val)
		(rec (cddr lis)))))))
(TEST
 > (sequencialpairs->pairs '(a 1) symbol? number?)
 ((a . 1))
 )


;; 'restargs-keyref' 'keyargs-maybe-ref'
(def (dsssl-maybe-ref args #(keyword? key))
     (let ((alis (sequencialpairs->pairs args keyword? true/1)))
       (eq-alist-maybe-ref alis key)))

(TEST
 > (dsssl-maybe-ref '(a: 1) a:)
 1
 > (dsssl-maybe-ref '(a: 1) b:)
 #f)


(def (dsssl-apply fn key-args . moreargs)
     (apply fn (append key-args moreargs)))


