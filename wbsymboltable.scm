;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.


(require easy
	 cj-cmp
	 joo
	 wbtable ;; implied baseclass loading anyone?
	 (wbtable wbtable:list->_)
	 (wbtree empty-wbtree)
	 test)

(export (joo-class wbsymboltable)
	;; wbsymboltable? implied in the above?
	wbsymboltable-of
	empty-wbsymboltable-of
	list->wbsymboltable-of
	list.wbsymboltable-of

	;; plus same method exports as wbtable, could we inherit those
	;; please? Or rather well really automatic, those for joo are
	;; not difficult? Except at runtime only.
	)


;; returns #t for structually equivalent table but possibly not
;; 'blessed into' wbsymboltable:

(def. (wbtable.wbsymboltable-compatible? v)
  ;; hm can't use wbtable-of, wow. Since that does eq? not
  ;; subtyping checking with the value? predicate.
  (=> v
      (.table-head)
      (.key?)
      (eq? symbol?)))


(joo-class
 ((wbsymboltable _wbsymboltable))
 ;; really want fewer fields, kinda, does that screem for containment
 ;; instead of inheritance (but really have all the methods to share)
 extends: wbtable

 ;; Here's our constructor with fewer fields:
 (def (wbsymboltable value? data)
      (_wbsymboltable (wbtable-head symbol? symbol-cmp value?)
		      data))
 ;; (XX why not wbsymboltable-of to be consistent with
 ;; empty-wbsymboltable-of, sigh?)

 (def (empty-wbsymboltable-of value?)
      (wbsymboltable value? empty-wbtree))


 (def (wbsymboltable-of value?)
      ;; XX Note that this one is (just strictly speaking?) also
      ;; actually broken, since subtyping is missing; value? may very
      ;; well return true for all of its values but not be eq? (well we
      ;; knew that about the non-eq-but-equivalent case anyway, but the
      ;; point is that the predicate doesn't even need to be type
      ;; equivalent, but subtype matching is enough, for it to remain
      ;; true.)

      ;; actually be satisfied with wbtable-of, i.e. (kinda?)
      ;; structural typing?
      ;;(wbtable-of symbol? value?)

      ;; or:
      (lambda (v)
	(and (wbsymboltable? v)
	     (=> v
		 (.table-head)
		 (.value?)
		 (eq? value?)))))




 (def (list.wbsymboltable-of l value?)
      (wbtable:list->_ (empty-wbsymboltable-of value?) l))

 ;; ~just for compat, well
 (def ((list->wbsymboltable-of value?) l)
      (list.wbsymboltable-of l value?))
 

 (def-method- (show t)
   `(list.wbsymboltable-of ,(.show (.list t))
			   ,(.show (=> t
				       (.table-head)
				       (.value?)))))

 )



(TEST
 > (def t (empty-wbsymboltable-of number?))
 > (def t2 (.add t 'y 10))
 > (def t3 (.add t2 'x 20.3))
 > (map .length (list t t2 t3))
 (0 1 2)
 > (map wbsymboltable? (list t t2 t3 empty-wbtree))
 (#t #t #t #f)
 > (%try-error (list.wbsymboltable-of '((z "2/3") (a . "a") (b . "b")) number?))
 #(error
   "assertment failure: ($value? (cdr key.val))"
   (number? (cdr '(z "2/3"))))
 ;; OH well? Ugly or actually okay?
 > (def u (list.wbsymboltable-of '((z . "2/3") (a . "a") (b . "b")) string?))
 > (.show u)
 (list.wbsymboltable-of (list (cons 'a "a")
			      (cons 'b "b")
			      (cons 'z "2/3"))
			string?)
 > (%try-error (.add u "q" "qq"))
 #(error
   "assertment failure: ($key? (car key.val))"
   (symbol? (car '("q" . "qq")))) 
 > (%try-error (.add u 'q 23))
 #(error
   "assertment failure: ($value? (cdr key.val))"
   (string? (cdr '(q . 23)))) 
 > (map (wbsymboltable-of string?) (list t t2 t3 empty-wbtree u))
 (#f #f #f #f #t)
 )

