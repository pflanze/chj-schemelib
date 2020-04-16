;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; This thing really has turned out ugly, both because
;; perhaps/probably it should be a contains relationship not
;; inheritance; and the useless wrapping is really something I should
;; solve now? (How?)


(require easy
	 joo
	 (predicates function?)
	 wbcollection

	 ;; ?
	 cj-cmp
	 (cj-source-quasiquote quasiquote-source)
	 (stream stream->list))

;; XX like in wbcollection, to make this class subclassable, sigh
(def (wbmcollection-change orig a b c)
     (if (wbcollection? orig)
	 (let ((c (vector-copy orig)))
	   (vector-set! c 1 a)
	   (vector-set! c 2 b)
	   (vector-set! c 3 c)
	   c)
	 (error "not a wbcollection:" orig)))


(joo-class
 ;; oh, should we instead have used a single sub-object holding all
 ;; values that remain constant over its operation life, well, now
 ;; that it's multiple? D'oh?
 (wbmcollection leaf-null
		#(function? leaf-cons))
 extends: wbcollection
 ;; leaf-cons receives (new-element, old leaf or leaf-null)

 
 (def-method- (add c item)
   (let-wbmcollection
    (($wbtreeparameter $data lnull lcons) c)

    (wbmcollection.data-set
     c
     (wbtree:set $data
		 (let ((group1 (lcons item lnull)))
		   ;; XX evil: maybe-ref disallows #f as leafs; why is
		   ;; there no wbtree:ref with nothing argument or so?
		   (cond ((wbtree:maybe-ref $data group1)
			  => (lambda (oldgroup)
			       (lcons item oldgroup)))
			 (else group1)))))))

 (def-method- (add-multiple c items)
   ;; (yeah, *could* be optimized minimally by avoiding object
   ;; allocations)
   (fold (lambda (v c)
	   (wbmcollection.add c v))
	 c
	 items))
 

 (def-method- (maybe-ref c item)
   (let-wbmcollection
    (($wbtreeparameter $data lnull lcons) c)
    ;; and the perennial fake wrapping
    (wbtree:maybe-ref $data (lcons item lnull))))
  
 (def-method- (set c item)
   (error "not implemented"))

 
 ;; (def-wbcollection-method (contains? c item)
 ;;   (error "not yet implemented"))

 ;; (def-wbcollection-method (maybe-ref c item)
 ;;   (wbtree:maybe-ref $data item))

 ;; ^ hmm these can actually work on the group?

 ;; this one shadow, since might be useful for deleting just the item
 ;; from the group? (Although does that mean we need another function
 ;; for that, uh! So stupid really, where OO shines, really, man?!)
 ;; Note: it will also need null? check, to remove the whole leaf.
 (def-method- (delete c item)
   (error "not implemented"))
 

 (def-method- delete-group ;; requires group not item !
   wbcollection.delete)
 ;; heh. as fast and still shadowed?


 (def-method- (show s show)
   (let-wbmcollection
    ((p _ lnull lcons) s)
    `(lists.wbmcollection
      ,(show (wbtreeparameter.cmp p))
      ,(show lnull)
      ,(show lcons)
      ,(show (.members s))))))



;; bad COPY-PASTE adaption:
(def (empty-wbmcollection #(function? cmp)
			  leaf-null
			  leaf-cons)
     ;; XX ah and here SUPER::new comes into play; I mean.
     (wbmcollection (wbtreeparameter* cmp)
		    empty-wbtree
		    leaf-null
		    leaf-cons))

;; ah and here COPY-PASTE actually stops, since it's not the same
;; principle anymore; should all of this module not be a subclass,
;; REALLY? Contains, not ISA?
;; Aha list vs lists.

(def (list.wbmcollection #(function? cmp)
			 leaf-null
			 leaf-cons
			 l)
     (.add-multiple (empty-wbmcollection cmp leaf-null leaf-cons)
		    l))


(def (lists.wbmcollection #(function? cmp)
			  leaf-null
			  leaf-cons
			  l)
     (let (($wbtreeparameter (wbtreeparameter* cmp)))
       (wbcollection $wbtreeparameter
		     (list->wbtree l))))

(TEST
 > (def tcmp (on caar real-cmp))
 > (def m (empty-wbmcollection tcmp '() cons))
 > (show m)
 (lists.wbmcollection tcmp (list) cons (list))
 > (def m (.add m '(1 . a)))
 > (show m)
 (lists.wbmcollection tcmp (list) cons (list (list (cons 1 'a)))) ;; XX actually wrong?F
 > (def m (.add m '(1 . b)))
 > (show m)
 (lists.wbmcollection tcmp (list) cons (list (list (cons 1 'b) (cons 1 'a))))
 > (def m (.add m '(2 . c)))
 > (show m)
 (lists.wbmcollection tcmp (list) cons
		      (list (list (cons 1 'b) (cons 1 'a))
			    (list (cons 2 'c))))
 > (def g (.maybe-ref m (cons 1 'X)))
 > (show g)
 (list (cons 1 'b) (cons 1 'a))
 > (show (.delete-group m g))
 (lists.wbmcollection tcmp (list) cons (list (list (cons 2 'c))))
 > (show (.delete-group m '((1 . x))))
 (lists.wbmcollection tcmp (list) cons (list (list (cons 2 'c))))
 > (def c (list.wbmcollection tcmp '() cons
			      '((1 . a)
				(2 . b)
				(3 . c)
				(1 . d)
				(3 . c))))
 > (.size c)
 3
 ;; hehe, not total number of items. This *REALLY* should be a
 ;; contains, not inherit, right? Not the same interface really
 ;; anymore (even if (mistakenly?) using the same method names).

 ;; Ah and here we still have got this ugly pointless wrapper thing,
 ;; forever, huh. "Why". Actually doubly, now.
 > (.maybe-ref c (cons 4 'X))
 #f
 > (.maybe-ref c (cons 1 'X))
 ((1 . d) (1 . a))
 > (.maybe-ref c (cons 2 'X))
 ((2 . b))
 > (.maybe-ref c (cons 3 'X))
 ((3 . c) (3 . c))
 )

