;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



;; "Just" wbcollections with an easier interface, which is also shown
;; with |.show|. Requires .cmp to be defined for the key type in
;; question.

(require easy
	 (cj-env on/registry on/registry-maybe-ref on/registry-ref)
	 (wbtree let-wbtreeparameter)
	 wbcollection
	 ;; included in easy?
	 show
	 test)

(export collection-on
	collection-on?
	(method collection-on.show))


(def ((collection-on .key) . items)
     (list.wbcollection (on/registry .key .cmp)
			items))

(def. symbol.cmp symbol-cmp)

(def collection-on?
     (both wbcollection?
	   (lambda (c)
	     (let-wbtreeparameter
	      ((cmp element?) (wbcollection.param c))
	      (cond ((on/registry-maybe-ref cmp)
		     => (lambda (p)
			  (eq? (cdr p) .cmp)))
		    (else #f))))))

(def. (collection-on.show v)
  (let* ((fullcmp (wbtreeparameter.cmp (wbcollection.param v)))
	 (p (on/registry-ref fullcmp))
	 (access (car p))
	 (cmp (cdr p)))
    ;; (assert (eq? (.show cmp) '.cmp))
    (assert (eq? cmp .cmp))
    `((collection-on ,(.show access))
      ,@(map .show (wbcollection.list v)))))


(TEST
 > (def. number.cmp number-cmp)
 > (def c ((collection-on car) (cons 4 "four") (cons 2 "two")))
 > (.show c)
 ((collection-on car) (cons 2 "two") (cons 4 "four")))

