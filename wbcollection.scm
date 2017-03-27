;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 joo
	 cj-cmp
	 (cj-source-quasiquote quasiquote-source)
	 (wbtree wbtree? _wbtree? empty-wbtree empty-wbtree?
		 wbtreeparameter*)
	 (stream stream->list stream-fold))

(export (joo-class wbcollection)
	empty-wbcollection
	list.wbcollection
	(joo-methods size
		     contains?
		     maybe-ref
		     min
		     max
		     add
		     add-multiple
		     delete
		     members list
		     members-stream stream
		     union
		     difference
		     intersection
		     intersection-stream
		     rank
		     index
		     show))


;; XX this, or some other solution, to be auto-generated by cj-struct
;; or joo, to make wbcollection subclassable. Ah, should also do type
;; checking of its arguments I suppose.
(def (wbcollection-change orig
			  #(wbtreeparameter? param)
			  #(wbtree? data))
     (if (wbcollection? orig)
	 (let ((c (vector-copy orig)))
	   (vector-set! c 1 param)
	   (vector-set! c 2 data)
	   c)
	 (error "not a wbcollection:" orig)))


(defmacro (wbcollection-lambda args . body)
  (match* args
	  ((c . _rest)
	   (quasiquote-source
	    (lambda ,args
	      (let-wbcollection
	       (($wbtreeparameter $data) ,c)
	       ,@body))))))

(defmacro (def-wbcollection-method name+args . body)
  (match* name+args
	  ((name . args)
	   (quasiquote-source
	    (def-method ,name
	      (wbcollection-lambda ,args
				   ,@body))))))


(joo-class
 (wbcollection #(wbtreeparameter? param)
	       #(wbtree? data))

 ;; (Methods which take an item can't type-check it: we don't
 ;; have a type predicate, just a comparison function. The type
 ;; predicate in wbtreeparameter? is only used by wbtree.scm to
 ;; distinguish leafs from branches.)

 ;; XX why not call this length (size being meant for capacity not
 ;; current number of elements, i.e. in C++ libraries?)?
 (def-wbcollection-method (size c)
   (wbtree:size $data))

 (def-wbcollection-method (empty? c)
   (empty-wbtree? $data))

 (def-wbcollection-method (contains? c item)
   (wbtree:member? $data item))

 (def-wbcollection-method (maybe-ref c item)
   (wbtree:maybe-ref $data item))

 ;; (def-wbcollection-method ( c)
 ;; 	 (wbtree:maybe-ref&rank $data))

 (def-wbcollection-method (min c)
   (wbtree:min $data))

 (def-wbcollection-method (max c)
   (wbtree:max $data))

 (def-wbcollection-method (maybe-min c)
   (wbtree:maybe-min $data))

 (def-wbcollection-method (maybe-max c)
   (wbtree:maybe-max $data))

 ;; `first` and `rest` don't really seem fitting here, since
 ;; there's no maintainance of insertion order, so leave it to
 ;; min and max.

 ;; XX is there a faster way to do this? (splitting)
 (def-wbcollection-method (min&rest c)
   (let ((*v (wbtree:min $data)))
     (values *v
	     (wbcollection-change c
				  $wbtreeparameter
				  (wbtree:delete $data *v)))))
 ;; copy-paste
 (def-wbcollection-method (max&rest c)
   (let ((*v (wbtree:max $data)))
     (values *v
	     (wbcollection-change c
				  $wbtreeparameter
				  (wbtree:delete $data *v)))))

 (def-wbcollection-method (set c item)
   (wbcollection-change c
			$wbtreeparameter
			(wbtree:set $data item)))

 (def-wbcollection-method (add c item)
   (wbcollection-change c
			$wbtreeparameter
			(wbtree:add $data item)))

 (def-wbcollection-method (add-multiple c items)
   (wbcollection-change c
			$wbtreeparameter
			(stream-fold (lambda (item data)
				       (wbtree:add data item))
				     $data
				     items)))

 (def-wbcollection-method (delete c item)
   (wbcollection-change c
			$wbtreeparameter
			(wbtree:delete $data item)))

 ;; wbtree:inorder-fold
 ;; wbtree:stream-inorder-fold
 ;; wbtree:inorder-fold-reverse
 ;; wbtree:stream-inorder-fold-reverse

 (def-wbcollection-method (members c)
   (wbtree:members $data))

 (def-method list wbcollection.members)

 (def-method (show s)
   `(list.wbcollection
     ,(.show (wbtreeparameter.cmp (.param s)))
     ,(.show (wbcollection.members s))))

 (def-wbcollection-method (members-stream c
					  #!optional
					  #(boolean? reverse?)
					  (tail '()))
   (if reverse?
       (wbtree:stream-inorder-fold-reverse $data cons tail)
       (wbtree:stream-inorder-fold $data cons tail)))

 (def-method stream wbcollection.members-stream)

 ;; wbtree:lt
 ;; wbtree:gt
 ;; wbtree:next

 (def (wbcollection:binary-method f return-wbcollection?)
      (wbcollection-lambda
       (c1 c2)
       (let-wbcollection
	(($wbtreeparameter2 $data2) c2)
	(if (wbtreeparameter-equal? $wbtreeparameter $wbtreeparameter2)
	    (let ((res (f $wbtreeparameter $data $data2)))
	      (if return-wbcollection?
		  (wbcollection $wbtreeparameter
				res)
		  res))
	    (error "incompatible wbcollection parameters:"
		   $wbtreeparameter (.param c2))))))
 
 (def-method union
   (wbcollection:binary-method wbtree:union* #t))

 (def-method difference
   (wbcollection:binary-method wbtree:difference* #t))

 (def-method intersection
   (wbcollection:binary-method wbtree:intersection* #t))

 (def-method intersection-stream
   (wbcollection:binary-method (lambda (param d1 d2)
				 (wbtrees:intersection-stream* param (list d1 d2)))
			       #f))

 ;; wbtree->stream
 ;; wbtree:between

 (def-wbcollection-method (rank c item)
   (wbtree:rank $data item))

 ;; rename this to `.ref` ? Or would that be dangerously close
 ;; to `.contains?` ?
 (def-wbcollection-method (index c item)
   (wbtree:index $data item)))


(def (empty-wbcollection #(function? cmp))
     (wbcollection (wbtreeparameter* cmp)
		   empty-wbtree))

(def (list.wbcollection #(function? cmp)
			l
			#!optional
			ignore-duplicates?)
     (let (($wbtreeparameter (wbtreeparameter* cmp)))
       (wbcollection $wbtreeparameter
		     (list->wbtree l ignore-duplicates?))))


(TEST
 ;; make sure there's no confusion with wbtee symbols
 > (def c (list.wbcollection generic-cmp '(1 9 -2 wbtree #(wbtree))))
 > (.list c)
 (-2 1 9 wbtree #(wbtree))
 > (def c (list.wbcollection generic-cmp '(1 9 -2 wbtree #(wbtree 1 2 3 4))))
 > (.list c)
 (-2 1 9 wbtree #(wbtree 1 2 3 4)))


(TEST
 > (.show
    (with-exception-catcher
     identity
     (& (list.wbcollection number-cmp '(1 3 2 9 -2 3.3 3)))))
 (wbtree-duplicate-exception 3 3)
 > (.list (list.wbcollection number-cmp '(1 3 2 9 -2 3.3 3) #t))
 (-2 1 2 3 3.3 9)
 > (def c (list.wbcollection number-cmp '(1 3 2 9 -2 3.3)))
 > (.contains? c 3)
 #t
 > (.contains? c 3.3)
 #t
 > (.contains? c 3.5)
 #f
 > (.list c)
 (-2 1 2 3 3.3 9)
 > (.min c)
 -2
 > (.max c)
 9
 > (wbtree-duplicate-exception?
    (with-exception-catcher identity (& (.add c 3))))
 #t
 > (def c2 (.add c 12))
 > (.max c2)
 12
 > (.max c)
 9
 > (.contains? (.delete c2 3) 3)
 #f
 > (map (C .rank c _) '(-2 1 2 3 3.3 9))
 (0 1 2 3 4 5)
 > (.rank c2 12)
 6
 > (with-exception-catcher identity (& (.rank c 12)))
 not-found ;; perhaps todo: also add .Maybe-rank

 ;; site and emptyness:
 > (def c (list.wbcollection number-cmp '(1 9)))
 > (.size c)
 2
 > (.empty? c)
 #f
 > (set! c (.delete c 1))
 > (.empty? c)
 #f
 > (.contains? c 9)
 #t
 > (.size c)
 1
 > (.min c)
 9
 > (.max c)
 9
 > (defvalues (m r) (.min&rest c2))
 > m
 -2
 > (.list r)
 (1 2 3 3.3 9 12)
 > (stream->list (.stream r))
 (1 2 3 3.3 9 12)
 > (stream->list (.stream r #t))
 (12 9 3.3 3 2 1)
 
 > (defvalues (m r) (.max&rest c2))
 > m
 12
 > (.list r)
 (-2 1 2 3 3.3 9)

 > (.show r)
 (list.wbcollection number-cmp (list -2 1 2 3 3.3 9))
 > (.show (list.wbcollection number-cmp (list -2 1 2 3 3.3 9)))
 (list.wbcollection number-cmp (list -2 1 2 3 3.3 9))
 
 ;; > (with-exception-catcher identity (& (set! c (.delete c 1))))
 ;; not-found  XXX why does this give #!void instead of exception?
 > (set! c (.delete c 9))
 > (.empty? c)
 #t
 > (.contains? c 9)
 #f
 > (.size c)
 0
 > (%try-error (.min c))
 ;; sigh, why different kind of error here?
 #(error "can't get min from empty wbtree")
 > (%try-error (.max c))
 #(error "can't get max from empty wbtree")

 > (.show c)
 (list.wbcollection number-cmp (list))
 )

;; Set operations
(TEST
 > (def (binarytest fn l1 l2 post)
	(let ((c (comp post
		       (on (C list.wbcollection number-cmp _)
			   fn))))
	  (list (c l1 l2)
		(c l2 l1))))
 > (binarytest .difference '(1 2 3) '(2 3 4) wbcollection.list)
 ((1) (4))
 > (binarytest .difference '(1 2 3) '(2 4) wbcollection.list)
 ((1 3) (4))
 > (binarytest .union '(1 2 3) '(2 4) wbcollection.list)
 ((1 2 3 4) (1 2 3 4))
 > (binarytest .intersection '(1 2 3) '(2 4) wbcollection.list)
 ((2) (2))
 > (binarytest .intersection-stream '(1 2 3) '(2 4) promise?)
 (#t #t)
 > (binarytest .intersection-stream '(1 2 3) '(2 4) stream->list)
 ((2) (2))
 > (binarytest .intersection-stream '(1 2 3) '(2 3 4) stream->list)
 ((2 3) (2 3))
 )
