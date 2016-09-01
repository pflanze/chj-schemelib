;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.


(require easy
	 show
	 joo
	 cj-cmp
	 wbtree
	 test)

(export (joo-class wbtable)
	wbtable?
	wbtable-of
	empty-wbtable-of
	list->wbtable-of
	;; list.wbtable-of  nah just too many args, k? ;; no list.wbtable, OK?
	;; list.wbtable-function
	(method
	 wbtable.length
	 wbtable.ref ;; with required alternative value if missing
	 wbtable.refx ;; exception
	 wbtable.contains?
	 wbtable.update
	 wbtable.fold
	 wbtable.every?
	 wbtable.list
	 wbtable.show
	 wbtable.keys
	 wbtable.sortedkeys
	 wbtable.update-all
	 wbtable.add
	 wbtable.set
	 wbtable.remove)
	#!optional
	(joo-class wbtable-head))



;; FINALLY give already in and make some sort of a TYPE? Kinda give
;; in only.

;; (joo-class (wbtable-key-type #(predicate? predicate?)
;; 			     #(function? cmp))
;; 	   (def-method (wbtreeparameter t)
;; 	     (wbtreeparameter (on car (.cmp t))
;; 			      pair?)))

;; Or not. 
;; "~since accessing the items through all these indirections is slow"
;; (Then going on to do one with the same number of indirections anyway.)


;; XX The real uglyness with the following is the possibility for
;; usage errors (cmp not matching key?).

(joo-class
 ((wbtable-head _wbtable-head)
  #(predicate? key?)
  #(function? key-cmp)
  #(predicate? value?)
  ;; and cached to avoid reallocating it:
  #(wbtreeparameter? wbtreeparameter))
 
 (def (wbtable-head key? key-cmp value?)
      (_wbtable-head key? key-cmp value?
		     (wbtreeparameter (on car key-cmp)
				      pair?)))

 (def-method (compatible? s #(wbtable-head? t))
   ;; pessimistic for now...
   (let-wbtable-head
    ((a b c _) s)
    (let-wbtable-head
     ((x y z _) t)
     (and (eq? a x)
	  (eq? b y)
	  (eq? c z))))))


(defmacro (def-wbtable-method name+args . body)
  ;; jeez the amount of code. Also hoping the compiler will optimize
  ;; away unused stuff....well. (pure 'tagging' needed)
  (match* name+args
	  ((name c . args)
	   (quasiquote-source
	    (def-method ,name+args
	      (let-wbtable
	       (($wbtable-head $data) ,c)
	       (let-wbtable-head (($key? $key-cmp $value? $wbtreeparameter)
				  $wbtable-head)
				 ,@body)))))))

(joo-class
 ((wbtable _wbtable)
  #(wbtable-head? table-head)
  #(wbtree? data))

 (def (wbtable key? key-cmp value?
	       data)
      (_wbtable (wbtable-head key? key-cmp value?)
		data))

 (def (empty-wbtable-of key? key-cmp value?)
      (wbtable key? key-cmp value? empty-wbtree))

 (def (wbtable-of key? value?)
      (lambda (v)
	(and (wbtable? v)
	     (let ((h (.table-head v)))
	       ;; XX function comparison by eq?: evil or ? (equal?
	       ;; really (of the type language?), REALLY.?!)
	       (and (eq? (.key? h) key?)
		    (eq? (.value? h) value?))))))

 ;; compare with:
 ;; (define* (list->wbtree l)
 ;;   (fold (lambda (x t)
 ;; 	  (wbtree:add t x))
 ;; 	empty-wbtree
 ;; 	l))

 (def ((list->wbtable-of key? key-cmp value?) l)
      ;; (assert ((list-of (pair-of key? value?)) l))
      ;; better: throw exception while iterating
      (fold (lambda (k.v t)
	      ;; re-use pairs, ah (dimly remember)
	      (.add-pair t k.v))
	    (empty-wbtable-of key? key-cmp value?)
	    l))


 ;; yeah, should really rename size in wbcollection ? !
 (def-wbtable-method (length s)
   (wbtree:size $data))

 (def-wbtable-method (empty? s)
   (empty-wbtree? $data))
 ;; ^ uh a case where the compiler really should optimize away unused
 ;; code wow. (Dead code elimination btw? Kinda.)

 (def-wbtable-method (maybe-ref-pair s #(pair? key.val))
   (assert ($key? (car key.val)))
   (wbtree:maybe-ref $data key.val))

 ;; XX gah dimly remember, too: useless cons. Should add some
 ;; maybe-ref-key to wbtree? Hm, how is cmp used, always in same
 ;; order?? todo.
 (def-method (ref s key alt)
   (cond ((wbtable.maybe-ref-pair s (cons key #f)) => cdr)
	 (else alt)))

 (def-method (refx s key)
   (cond ((wbtable.maybe-ref-pair s (cons key #f)) => cdr)
	 (else (error "key not found:" key))))

 ;; restrict `contains?` to collections, i.e. full items? And follow
 ;; Perl with `exists`, OK?
 (def-method (exists? s key)
   (and (wbtable.maybe-ref-pair s (cons key #f))
	#t))

 ;; XX wbtable.update

 ;; XX wbtable.fold

 ;; wbtable.every?

 (def-wbtable-method (list s)
   (wbtree:members $data))

 (def-wbtable-method (show s)
   `((list->wbtable-of ,(.show $key?)
		       ,(.show $key-cmp)
		       ,(.show $value?))
     (list ,@(map .show (.list s)))))

 ;; wbtable.keys
 ;; wbtable.sortedkeys  same ?

 ;; wbtable.update-all  what was that?

 (def-wbtable-method (add-pair s #(pair? key.val))
   (assert ($key? (car key.val)))
   (assert ($value? (cdr key.val)))

   (.data-set s (wbtree:add $data key.val)))

 (def-wbtable-method (set-pair s #(pair? key.val))
   (assert ($key? (car key.val)))
   (assert ($value? (cdr key.val)))

   (.data-set s (wbtree:set $data key.val)))

 ;; call it remove as symboltable or delete as wbcollection, wbtree,
 ;; Perl?
 (def-wbtable-method (delete-pair s #(pair? key.val))
   (assert ($key? (car key.val)))

   (.data-set s (wbtree:delete $data key.val)))

 ;; ^ XX what about missing keys here? Compatibility with symboltable,
 ;; or?
 
 (def-method (add s key val)
   (wbtable.add-pair s (cons key val)))

 (def-method (set s key val)
   (wbtable.set-pair s (cons key val)))

 (def-method (delete s key)
   (wbtable.delete-pair s (cons key #f)))

 ;; XX die or not on conflicting elements?
 (def-wbtable-method (union s t)
   (let ((h1 (.table-head s))
	 (h2 (.table-head t)))
     (if (.compatible? h1 h2)
	 (.data-set s
		    (wbtree:union $data
				  (.data t)))
	 ;; worry about huge data or not, forever? only show heads?
	 (error "incompatible table heads:"
		;; and do an error that does .show implicitely rather
		;; than use .show here, k?
		h1 h2))))
 
 )


(TEST
 > (def t (empty-wbtable-of symbol? symbol-cmp integer?))
 > (def t2 (=> t
	       (.add 'x 1)
	       (.add 'y 2)))
 > (.exists? t2 'x)
 #t
 > (.exists? t2 'y)
 #t
 > (.exists? t2 'z)
 #f
 > (def t3 (.delete t2 'x))
 > (.exists? t3 'x)
 #f
 > (.exists? t3 'y)
 #t
 > (.refx t3 'y)
 2
 > (%try-error (.refx t3 'x))
 #(error "key not found:" x)
 > (.ref t3 'x 'no)
 no
 > (map .length (list t t2 t3))
 (0 2 1)
 > (map .empty? (list t t2 t3))
 (#t #f #f)
 > (map (wbtable-of symbol? integer?)
	(list t t2 t3 't (empty-wbtable-of symbol? symbol-cmp string?)))
 (#t #t #t #f #f)
 > (.show t2)
 ((list->wbtable-of symbol? symbol-cmp integer?)
  (list (cons 'x 1)
	(cons 'y 2)))

 > (def u ((list->wbtable-of symbol? symbol-cmp integer?)
	   '((a . 11) (b . 12) (x . 10))))
 > (def u2 (.union t2 u))
 > (.show u2)
 ((list->wbtable-of symbol? symbol-cmp integer?)
  (list (cons 'a 11)
	(cons 'b 12)
	(cons 'x 10)
	(cons 'y 2)))
 )