;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Unlike wbcollection this separates .key and .eq? functions (instead
;; of having them grouped into one function, e.g. through |on|), and
;; thus can offer lookups by keys instead of whole objects.

;; "Just" wbcollections with an easier interface, which is also shown
;; with |.show|. Requires .cmp to be defined for the key type in
;; question.

(require easy
	 wbtree
	 show ;; included in easy?
	 test
	 ;; XX UNFINISHED, REMOVE?
	 (wbcollection wbcollection-lambda
		       wbcollection-change))


(export (class collection-on)
	;; ^ could that already include all the methods, at least
	;; those within the syntactical form of the class definition ?
	)



(jclass ((collection-on _collection-on)
	 #(function? .key)
	 #(function? key-cmp)
	 #((maybe function?) of) ;; item?
	 #(wbtreeparameter? $wbtreeparameter)
	 #(wbtree? data))

	(def ((collection-on .key key-cmp
			     #!key
			     #((maybe function?) of)
			     ;; XX hmm the following only used during
			     ;; creation, does that make any sense?
			     #(boolean? ignore-duplicates?))
	      . items)
	     (let ((param (wbtreeparameter (on .key key-cmp)
					   pair?)))
	       (_collection-on
		.key
		key-cmp
		of
		param
		(list->wbtree* param
			       (map (lambda (item)
				      (if (or (not of)
					      (of item))
					  (cons (.key item) item)
					  (error "item does not match type predicate:"
						 item
						 of)))
				    items)
			       ignore-duplicates?))))


	;; Now just a big modified COPY-PASTE from wbcollection.scm
	;; (don't need anything like def-wbcollection-method any more
	;; since def-method* now does the same already for us, just
	;; named the param field $wbtreeparameter already):

	(def-method* (size c)
	  (wbtree:size data))

	(def-method* (empty? c)
	  (empty-wbtree? data))

	(def-method* (contains? c item)
	  (wbtree:member? data item))

	;; call it ref-item versus just ref here, unlike in
	;; wbcollection
	(def-method* (maybe-ref-item c item)
	  (wbtree:maybe-ref data item))

	(def-method* (maybe-ref c key)
	  (wbtree:maybe-ref data (cons key #f)))

	(def-method* (ref c key)
	  (wbtree:ref data (cons key #f)))

	
	;; (def-method* ( c)
	;; 	 (wbtree:maybe-ref&rank data))

	(def-method* (min c)
	  (wbtree:min data))

	(def-method* (max c)
	  (wbtree:max data))

	(def-method* (maybe-min c)
	  (wbtree:maybe-min data))

	(def-method* (maybe-max c)
	  (wbtree:maybe-max data))

	;; `first` and `rest` don't really seem fitting here, since
	;; there's no maintainance of insertion order, so leave it to
	;; min and max.

	;; XX is there a faster way to do this? (splitting)
	(def-method* (min&rest c)
	  (let ((*v (wbtree:min data)))
	    (values *v
		    (wbcollection-change c
					 $wbtreeparameter
					 (wbtree:delete data *v)))))
	;; copy-paste
	(def-method* (max&rest c)
	  (let ((*v (wbtree:max data)))
	    (values *v
		    (wbcollection-change c
					 $wbtreeparameter
					 (wbtree:delete data *v)))))

	(def-method* (set c item)
	  (wbcollection-change c
			       $wbtreeparameter
			       (wbtree:set data item)))

	(def-method* (add c item)
	  (wbcollection-change c
			       $wbtreeparameter
			       (wbtree:add data item)))

	(def-method* (add-multiple c items)
	  (wbcollection-change c
			       $wbtreeparameter
			       (stream-fold (lambda (item data)
					      (wbtree:add data item))
					    data
					    items)))

	(def-method* (delete c item)
	  (wbcollection-change c
			       $wbtreeparameter
			       (wbtree:delete data item)))

	;; wbtree:inorder-fold
	;; wbtree:stream-inorder-fold
	;; wbtree:inorder-fold-reverse
	;; wbtree:stream-inorder-fold-reverse

	(def-method* (members c)
	  (wbtree:members data))

	(def-method- list wbcollection.members)

	(def-method* (show s)
	  `((collection-on ,(.show .key)
			   ,(.show key-cmp)
			   ,@(if of
				 (list of: of)
				 `()))
	    ,@(.members s)))

	(def-method* (members-stream c
				     #!optional
				     #(boolean? reverse?)
				     (tail '()))
	  (if reverse?
	      (wbtree:stream-inorder-fold-reverse data cons tail)
	      (wbtree:stream-inorder-fold data cons tail)))

	(def-method- stream wbcollection.members-stream)

	;; wbtree:lt
	;; wbtree:gt
	;; wbtree:next

	(def (wbcollection:binary-method f return-wbcollection?)
	     (wbcollection-lambda
	      (c1 c2)
	      (let-wbcollection
	       (($wbtreeparameter2 data2) c2)
	       (if (wbtreeparameter-equal? $wbtreeparameter $wbtreeparameter2)
		   (let ((res (f $wbtreeparameter data data2)))
		     (if return-wbcollection?
			 (wbcollection $wbtreeparameter
				       res)
			 res))
		   (error "incompatible wbcollection parameters:"
			  $wbtreeparameter (.param c2))))))
 
	(def-method- union
	  (wbcollection:binary-method wbtree:union* #t))

	(def-method- difference
	  (wbcollection:binary-method wbtree:difference* #t))

	(def-method- intersection
	  (wbcollection:binary-method wbtree:intersection* #t))

	(def-method- intersection-stream
	  (wbcollection:binary-method (lambda (param d1 d2)
					(wbtrees:intersection-stream* param (list d1 d2)))
				      #f))

	;; wbtree->stream -- have members-stream aka stream already; XX heh,
	;;   code duplication in wbtree already

	;; wbtree:between

	(def-method* (rank c item)
	  (wbtree:rank data item))

	;; rename this to `.ref` ? Or would that be dangerously close
	;; to `.contains?` ?
	(def-method* (index c item)
	  (wbtree:index data item)))


;; (def ((list.collection-on key key-cmp) . items)
;;      )


;;-----------------------------------------------------------------------------

;; (def ((collection-on .key) . items)
;;      (list.wbcollection (on/registry .key .cmp)
;; 			items))

;; (def. symbol.cmp symbol-cmp)

;; (def collection-on?
;;      (both wbcollection?
;; 	   (lambda (c)
;; 	     (let-wbtreeparameter
;; 	      ((cmp element?) (wbcollection.param c))
;; 	      (cond ((on/registry-maybe-ref cmp)
;; 		     => (lambda (p)
;; 			  (eq? (cdr p) .cmp)))
;; 		    (else #f))))))

;; (def. (collection-on.show v)
;;   (let* ((fullcmp (wbtreeparameter.cmp (wbcollection.param v)))
;; 	 (p (on/registry-ref fullcmp))
;; 	 (access (car p))
;; 	 (cmp (cdr p)))
;;     ;; (assert (eq? (.show cmp) '.cmp))
;;     (assert (eq? cmp .cmp))
;;     `((collection-on ,(.show access))
;;       ,@(map .show (wbcollection.list v)))))


;; (TEST
;;  > (def. number.cmp number-cmp)
;;  > (def c ((collection-on car) (cons 4 "four") (cons 2 "two")))
;;  > (.show c)
;;  ((collection-on car) (cons 2 "two") (cons 4 "four")))


(TEST
 > (def c ((collection-on first number-cmp) '(1 a) '(3 b) '(2 x)))
 
 )