;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy cj-alist list-util)


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


(def (dsssl-ref args #(keyword? key) alternative)
     (let lp ((vs args))
       (if (null? vs)
	   alternative
	   (let-pair ((k vs*) vs)
		     (if (null? vs)
			 (error "uneven argument count in:" args)
			 (let-pair ((v vs**) vs*)
				   (if (eq? key k)
				       v
				       (lp vs**))))))))

(TEST
 > (def vs '(a: 1 b: 2 b: 3 c: 4))
 > (dsssl-ref vs b: 'no)
 2
 > (dsssl-ref vs x: 'no)
 no)

;; and since the above was not enough..
(TEST
 > (dsssl-ref '(a: #f b: #f c: #t) a: 'no)
 #f
 > (dsssl-ref '(a: #f b: #f c: #t) b: 'no)
 #f
 > (dsssl-ref '(a: #f b: #f c: #t) c: 'no)
 #t
 > (dsssl-ref '(a: #f b: #f c: #t) d: 'no)
 no)


(def (dsssl-delete args #(keyword? key))
     (let rec ((vs args))
       (if (null? vs)
	   vs
	   (let-pair ((k vs*) vs)
		     (if (null? vs)
			 (error "uneven argument count in:" args)
			 (let-pair ((v vs**) vs*)
				   (if (eq? key k)
				       (rec vs**)
				       (cons* k v (rec vs**)))))))))

(TEST
 > (def vs '(a: 1 b: 2 b: 3 c: 4))
 > (dsssl-delete vs x:)
 (a: 1 b: 2 b: 3 c: 4)
 > (dsssl-delete vs c:)
 (a: 1 b: 2 b: 3)
 > (dsssl-delete vs a:)
 (b: 2 b: 3 c: 4)
 > (dsssl-delete vs b:)
 (a: 1 c: 4))


(def (dsssl-apply fn key-args . moreargs)
     (apply fn (append key-args moreargs)))


