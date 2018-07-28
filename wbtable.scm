;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.


(require easy
	 show
	 cj-cmp
	 wbtree
	 test)

(export (class wbtable)
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
	(class wbtable-head)
	wbtable:list->_)



;; FINALLY give already in and make some sort of a TYPE? Kinda give
;; in only.

;; (defclass (wbtable-key-type #(predicate? predicate?)
;; 			     #(function? cmp))
;; 	   (def-method- (wbtreeparameter t)
;; 	     (wbtreeparameter (on car (.cmp t))
;; 			      pair?)))

;; Or not. 
;; "~since accessing the items through all these indirections is slow"
;; (Then going on to do one with the same number of indirections anyway.)



;; make accessing the (nested) fields easy (defmethod would allow
;; access to data easily, but not the nested ones)
(defmacro (@with-wbtable s vars . body)
  (def exprs `((key? (@wbtable-head.key? $table-head))
	       (key-cmp (@wbtable-head.key-cmp $table-head))
	       (value? (@wbtable-head.value? $table-head))
	       ($wbtreeparameter (@wbtable-head.wbtreeparameter $table-head))
	       (data (@wbtable.data ,s))))
  (assert* symbol? s) ;; since I'm not eval-ing to a var
  (assert*
   list? vars
   (lambda (vars*)
     `(let (($table-head (@wbtable.table-head ,s)))
	(let ,(map (lambda (var)
		     (assert* symbol? var
			      (lambda (var*)
				(if-let ((expr (assq var* exprs)))
					;; heh use key and val right together
					expr
					(source-error var "request for a var I don't know about")))))
		   vars*)
	  
	  ,@body)))))



;; XX The real uglyness with the following is the possibility for
;; usage errors (cmp not matching key?).

(defclass ((wbtable-head _wbtable-head)
	   #(predicate? key?)
	   #(function? key-cmp)
	   #(predicate? value?)
	   ;; and cached to avoid reallocating it:
	   #(wbtreeparameter? wbtreeparameter))
 
  (def (wbtable-head key? key-cmp value?)
       (_wbtable-head key? key-cmp value?
		      (wbtreeparameter (on car key-cmp)
				       pair?)))

  (def-method- (compatible? s t)
    ;; pessimistic for now...
    (let-wbtable-head
     ((a b c _) s)
     (let-wbtable-head
      ((x y z _) t)
      (and (eq? a x)
	   (eq? b y)
	   (eq? c z))))))



(defclass ((wbtable _wbtable)
	   #(wbtable-head? table-head)
	   #(wbtree? data))

  (def (wbtable key? key-cmp value?
		data)
       (_wbtable (wbtable-head key? key-cmp value?)
		 data))

  (def (empty-wbtable-of key? key-cmp value?)
       (wbtable key? key-cmp value? empty-wbtree))

  (def (wbtable-of _key? _value?)
       (lambda (v)
	 (and (wbtable? v)
	      (@with-wbtable
	       v (key? value?)
	       ;; XX function comparison by eq?: evil or ? (equal?
	       ;; really (of the type language?), REALLY.?!)
	       (and (eq? key? _key?)
		    (eq? value? _value?))))))

  ;; compare with:
  ;; (define* (list->wbtree l)
  ;;   (fold (lambda (x t)
  ;; 	  (wbtree:add t x))
  ;; 	empty-wbtree
  ;; 	l))

  ;; (assert ((list-of (pair-of key? value?)) l))
  ;; better: throw exception while iterating

  ;; make re-usable by inheriting classes..:
 
  (def (wbtable:list->_ empty l)
       (fold (lambda (k.v t)
	       ;; re-use pairs, ah (dimly remember)
	       (wbtable.add-pair t k.v))
	     empty
	     l))
 
  (def ((list->wbtable-of key? key-cmp value?) l)
       (wbtable:list->_ (empty-wbtable-of key? key-cmp value?) l))
 


  ;; yeah, should really rename size in wbcollection ? !
  (defmethod- (length s)
    (@with-wbtable s ($wbtreeparameter data)
		   (wbtree:size data)))

  (defmethod (empty? s)
    (empty-wbtree? data))

  (defmethod- (maybe-ref-pair s #(pair? key.val))
    (@with-wbtable s (key? $wbtreeparameter data)
		   (assert (key? (car key.val)))
		   (wbtree:maybe-ref data key.val)))

  ;; XX gah dimly remember, too: useless cons. Should add some
  ;; maybe-ref-key to wbtree? Hm, how is cmp used, always in same
  ;; order?? todo.
  (defmethod- (ref s key alt)
    (cond ((wbtable.maybe-ref-pair s (cons key #f)) => cdr)
	  (else alt)))

  (defmethod- (refx s key)
    (cond ((wbtable.maybe-ref-pair s (cons key #f)) => cdr)
	  (else (error "key not found:" key))))

  ;; restrict `contains?` to collections, i.e. full items? And follow
  ;; Perl with `exists`, OK?
  (defmethod- (exists? s key)
    (and (wbtable.maybe-ref-pair s (cons key #f))
	 #t))

  (def-method- (update s key fn initial-value-thunk)
    (@with-wbtable
     s ($wbtreeparameter data)
     (if-let ((kv (wbtree:maybe-ref data (cons key #f))))
	     (let* ((v (cdr kv))
		    (v* (fn v)))
	       (if (eq? v v*)
		   s
		   (wbtable.data-set s (wbtree:set data (cons key v*)))))
	     (wbtable.data-set
	      s (wbtree:set data (cons key (fn (initial-value-thunk))))))))

  ;; XX wbtable.fold

  ;; wbtable.every?

  (defmethod- (list s)
    (@with-wbtable
     s ($wbtreeparameter data)
     (wbtree:members data)))

  (defmethod- (show s)
    (@with-wbtable
     s (key? key-cmp value?)
     (if (.empty? s)
	 `(empty-wbtable-of ,(.show key?)
			    ,(.show key-cmp)
			    ,(.show value?))
	 `((list->wbtable-of ,(.show key?)
			     ,(.show key-cmp)
			     ,(.show value?))
	   (list ,@(map .show (.list s)))))))

  ;; wbtable.keys
  ;; wbtable.sortedkeys  same ?

  ;; wbtable.update-all  what was that?

  (defmethod- (add-pair s #(pair? key.val))
    (@with-wbtable
     s ($wbtreeparameter key? value? data)

     (assert (key? (car key.val)))
     (assert (value? (cdr key.val)))

     (wbtable.data-set s (wbtree:add data key.val))))

  (defmethod- (set-pair s #(pair? key.val))
    (@with-wbtable
     s ($wbtreeparameter key? value? data)
    
     (assert (key? (car key.val)))
     (assert (value? (cdr key.val)))

     (wbtable.data-set s (wbtree:set data key.val))))

  ;; call it remove as symboltable or delete as wbcollection, wbtree,
  ;; Perl?
  (defmethod- (delete-pair s #(pair? key.val))
    (@with-wbtable
     s ($wbtreeparameter key? data)

     (assert (key? (car key.val)))

     (wbtable.data-set s (wbtree:delete data key.val))))

  ;; ^ XX what about missing keys here? Compatibility with symboltable,
  ;; or?
 
  (defmethod- (add s key val)
    (wbtable.add-pair s (cons key val)))

  (defmethod- (set s key val)
    (wbtable.set-pair s (cons key val)))

  (defmethod- (delete s key)
    (wbtable.delete-pair s (cons key #f)))

  ;; XX die or not on conflicting elements?
  (defmethod- (union s t)
    (let ((h1 (wbtable.table-head s))
	  (h2 (wbtable.table-head t)))
      (if (wbtable-head.compatible? h1 h2)
	  (wbtable.data-set
	   s
	   (@with-wbtable
	    s ($wbtreeparameter data)
	    (wbtree:union data
			  (wbtable.data t))))
	  ;; worry about huge data or not, forever? only show heads?
	  (error "incompatible table heads:"
		 ;; and do an error that does .show implicitely rather
		 ;; than use .show here, k?
		 h1 h2)))))


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
 > (%try (.add t3 'y 3))
 (exception
  text:
  "This object was raised: [(wbtree-duplicate-exception) (y . 2) (y . 3)]\n")
 > (.refx (.set t3 'y 3) 'y)
 3
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
	(cons 'y 2))))


(TEST
 > (def t (empty-wbtable-of string? string-cmp any?))
 > (.ref t "foo" 'nope)
 nope
 > (def t (.update t "foo" inc-function (& 10)))
 > (.ref t "foo" 'nope)
 11
 > (def t (.update t "foo" inc-function (& 10)))
 > (.ref t "foo" 'nope)
 12)

