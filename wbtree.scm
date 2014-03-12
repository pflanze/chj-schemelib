;;; Copyright 2010 by Christian Jaeger <chrjae@gmail.com>
;;; based on a text (c) University of Southampton (see below)

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block)
	 ;;(not safe)
	 )

;; Partially translated from code in ML shown in:
;;  Implementing Sets Efficiently in a Functional Language
;;  Stephen Adams
;;  CSTR 92-10
;;  (c) University of Southampton

; type Element
; datatype Tree = E | T of Element * int * Tree * Tree

; (define-struct element
;   key
;   value)
;or let that be the user's choice



;; turn multiple cmps into a new cmp, that compares by the cmps in
;; order of the list until one not returning eq is found
(define (2cmp cmp1 cmp2)
  (lambda (a b)
    (match-cmp (cmp1 a b)
	       ((lt) 'lt)
	       ((gt) 'gt)
	       ((eq) (cmp2 a b)))))

(define cmp-always-eq
  (lambda (a b)
    'eq))

(define (list-cmps->cmp cmps)
  (fold-right 2cmp
	      cmp-always-eq
	      cmps))

(define (cmps->cmp . cmps)
  (list-cmps->cmp cmps))

;; (TEST
;;  )


(define-struct treeparameter
  cmp
  element?)

(define (treeparameter:wrap-in tp type? access)
  (let-treeparameter
   ((cmp _) tp) ;; ignoring original |element?| check (not going to nest it)
   (make-treeparameter (on access cmp)
		       type?)))

;; thread param through functions: using macros to reduce the amount
;; of code that needs to be changed:

;; using |$treeparameter| as parameter variable name everywhere.

(define-macro* (define* name+vars . body)
  (match* name+vars
	  ((name . vars)
	   (let ((name* (symbol-append (source-code name) "*")))
	     ;; at the same time, define a macro for the nonstarred name
	     `(begin
		(define-macro* (,name . argexprs)
		  (cons ',name*
			(cons '$treeparameter
			      argexprs)))
		(define (,name* ,@(cons '$treeparameter vars))
		  ;; locally define |cmp| and |element?|:
		  (let-treeparameter
		   ((cmp element?) $treeparameter)
		   ,@body)))))))

;; to turn a function that has manually been defined with name ending
;; in * and taking $treeparameter as first argument into a macro:
(define-macro* (define-define*s . names)
  `(begin
     ,@(map (lambda (name)
	      (let ((name* (symbol-append (source-code name) "*")))
		`(define-macro* (,name . argexprs)
		   (cons ',name*
			 (cons '$treeparameter
			       argexprs)))))
	    (source-code names))))



(define weight 2);;XXX how parametrize this too


(define empty-tree 'empty-tree)
(define (empty-tree? x)
  (eq? x empty-tree))

(compile-time
 (define use-trees-as-leafs? #f))

(IF use-trees-as-leafs?
    (begin ;; original variant
      (define-struct tree
	;; constructor-name: _make-tree
	;; hm ne eben alle umdeffen muss hei. wl so sei.
	element	;; element
	size ;; int
	left ;; tree
	right ;; tree
	))
    (begin ;; optimized variant
      (define-struct tree
	;; constructor-name: _make-tree
	;; hm ne eben alle umdeffen muss hei. wl so sei.
	prefix: "_"
	generic-accessor-prefix: "tree-"
	let*-name: let*-tree
	element	;; element
	size ;; int
	left ;; tree
	right ;; tree
	)

      ;; dynamic dispatch, to save wrapping the leaf elements in trees:
      (define (make-tree e s l r)
	(if (and (eq? l empty-tree)
		 (eq? r empty-tree))
	    e
	    (_make-tree e s l r)))

					; (define (_access accessor)
					;   (lambda (v)
					;     (if (element? v)
					; 	ah sh alle verschieden)))

      ;; (hm no need for runtime macro here hm. ?. localized would suffice)
      (define-macro* (_access access-element access-tree)
	`(lambda (v)
	   (if (element? v)
	       (,access-element v)
	       (,access-tree v))))

      (define* (tree-element v)
	((_access identity _tree-element) v))
      (define* (tree-size v)
	((_access (lambda (v)
		   1)
		 _tree-size) v))
      (define* (tree-left v)
	((_access (lambda (v)
		   empty-tree)
		 _tree-left) v))
      (define* (tree-right v)
	((_access (lambda (v)
		   empty-tree)
		 _tree-right) v))
      ;; / dynamic dispatch
      ))

(define* (tree:size t)
  (cond ((empty-tree? t)
	 0)
	(else
	 (tree-size t))))

;; fun N(v,l,r) = T(v, 1 + size l + size r, l, r)
(define* (new-tree v l r)
  (make-tree v
	     (+ 1
		(tree:size l)
		(tree:size r))
	     l
	     r))

(IF #f
    (begin ;; C variant, see wbtree-C
      )
    (begin ;; Scheme variant
      (define* (tree:member? t x)
	(let member? ((t t))
	  (cond ((empty-tree? t)
		 #f)
		(else
		 (let ((v (tree-element t)))
		   (match-cmp (cmp x v)
		     ((lt) (member? (tree-left t)))
		     ((gt) (member? (tree-right t)))
		     ((eq) #t)))))))

      ;; and with 1 *single* change:
      (define* (tree:maybe-ref t x)
	(let member? ((t t))
	  (cond ((empty-tree? t)
		 #f)
		(else
		 (let ((v (tree-element t)))
		   (match-cmp (cmp x v)
		     ((lt) (member? (tree-left t)))
		     ((gt) (member? (tree-right t)))
		     ((eq) v)))))))
      ))

;; variant that also returns the position:
;; (they call it rank, not position nor index; tree:index goes from the index to the element)
(define* (tree:maybe-ref&rank t x)
  (let member? ((t t)
		(nleft 0))
    (cond ((empty-tree? t)
	   #f)
	  (else
	   (let-tree ((v _ l _) t)
		     (let ((nleft* (lambda ()
				     (+ nleft
					(tree:size l)))))
		       (match-cmp
			(cmp x v)
			((lt) (member? l
				       nleft))
			((gt) (member? (tree-right t)
				       (+ (nleft*) 1)))
			((eq) (cons v
				    (nleft*))))))))))


(define* (_tree-min _tree-left*)
  (lambda (t)
    (let rec ((t t))
      (cond ((empty-tree? t)
	     (error "can't get min from empty tree"))
	    (else
	     (let ((l (_tree-left* $treeparameter t)))
	       (cond ((empty-tree? l)
		      (tree-element t))
		     (else
		      (rec l)))))))))

(define* (tree:min t)
  ((_tree-min tree-left*) t))

(define* (tree:max t)
  ((_tree-min tree-right*) t))

; fun single_L (a,x,T(b,_,y,z))         = N(b,N(a,x,y),z)
; fun single_R (b,T(a,_,x,y),z)         = N(a,x,N(b,y,z))

(define* (single-l a x t)
  (let-tree ((b _ y z) t)
	    (new-tree b
		      (new-tree a
				x
				y)
		      z)))

(define* (single-r a t x) ;; z
  ;; ^ -- transp
  (let-tree ((b _ y z) t)
	    ;; (b _ x y)
	    (new-tree b
		      y	;;hrm nid z
		      ;; -- transp
		      (new-tree a
				z ;; hrm nid y  also diese auch noch vertauschn.
				;; --auch noch transp
				x))))


; fun double_L (a,x,T(c,_,T(b,_,y1,y2),z)) =
;                               N(b,N(a,x,y1),N(c,y2,z))

(define* (double-l a x t1)
  (let*-tree (((c _ t2 z) t1)
	      ((b _ y1 y2) t2))
	     (new-tree b
		       (new-tree a x y1)
		       (new-tree c y2 z))))

; fun double_R (c,T(a,_,x,T(b,_,y1,y2)),z) =
;                               N(b,N(a,x,y1),N(c,y2,z))

(define* (double-r c t1 z)
  (let*-tree (((a _ x t2) t1)
	      ((b _ y1 y2) t2))
	     (new-tree b
		       (new-tree a x y1)
		       (new-tree c y2 z))))


(define* (T* v l r)
  (let ((ln (tree:size l))
	(rn (tree:size r)))
    (if (< (+ ln rn) 2)
	(new-tree v l r)
	(if (> rn (* weight ln)) ;;XX integer ops?
	    ;; right is too big
	    (let* ((rl (tree-left r))
		   (rr (tree-right r))
		   (rln (tree:size rl))
		   (rrn (tree:size rr)))
	      (if (< rln rrn)
		  (single-l v l r)
		  (double-l v l r)))
	    (if (> ln (* weight rn))
		;; left is too big
		(let* ((ll (tree-left l))
		       (lr (tree-right l))
		       (lln (tree:size ll))
		       (lrn (tree:size lr)))
		  (if (< lrn lln)
		      (single-r v l r)
		      (double-r v l r)))
		(new-tree v l r))))))

;; a better name for this would probably be tree:set
(define* (tree:add t x)
  (let add ((t t))
    (cond ((empty-tree? t)
	   (make-tree x 1 empty-tree empty-tree))
	  (else
	   (let-tree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (T* v (add l) r))
		       ((gt) (T* v l (add r)))
		       ((eq)
			;; already there XXX
			;; -- ps.  auch den eq? trick andwenden?  oder eben just error out? ps what about contains etc. .
			;;t
			(new-tree x l r))))))))

; (define (tree:_delete* l r)
;   (cond ((empty-tree? l)
; 	 r)
; 	((empty-tree? r)
; 	 l)
; 	(else
; 	 (let ((min-elt (tree:min r)))
; 	   (T* min-elt l (tree:delete r min-elt))))))

;;^ *not efficient*

(define* (tree:_delmin t)
  (cond ((empty-tree? (tree-left t))
	 (tree-right t))
	(else
	 (let-tree ((v _ l r) t)
		   (T* v (tree:_delmin l) r)))))

(define* (tree:delete t x)
  
  (define (tree:_delete* l r)
    (cond ((empty-tree? l)
	   r)
	  ((empty-tree? r)
	   l)
	  (else
	   (T* (tree:min r) l (tree:_delmin r)))))

  (let delete ((t t))
    (cond ((empty-tree? t)
	   ;; XXX die instead?
	   empty-tree)
	  (else
	   (let-tree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (T* v (delete l) r))
		       ((gt) (T* v l (delete r)))
		       ((eq) (tree:_delete* l r))))))))


;;(define (tree:inorder-fold f base tree)

(define-macro* (define-strict-and-lazy
		 strict-name
		 stream-name
		 expr)
  ;; replaces |DELAY| with delay or 'nothing', same for |FV| and |FORCE|
  `(begin
     (define ,strict-name
       (let ()
	(##define-syntax DELAY
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((DELAY expr)
			      expr))
			    stx)))
	(##define-syntax FV
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((FV vars . body)
			      `(begin
				 ,@body)))
			    stx)))
	(##define-syntax FORCE
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((FORCE expr)
			      expr))
			    stx)))
	,expr))
     (define ,stream-name
       (let ()
	(##define-syntax DELAY
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((DELAY expr)
			      `(delay ,expr)))
			    stx)))
	(##define-syntax FV
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((FV vars . body)
			      `(let ,(map (lambda (v)
					    `(,v (force ,v)))
					  (source-code vars))
				 ,@body)))
			    stx)))
	(##define-syntax FORCE
			 (lambda (stx)
			   (cj-sourcify-deep
			    (match*
			     stx
			     ((FORCE expr)
			      `(force ,expr)))
			    stx)))
	,expr))))

(define-strict-and-lazy
  tree:inorder-fold_
  tree:stream-inorder-fold_

  (lambda (possiblyflip)
    (lambda ($treeparameter t f base)
      (letrec ((fold* (lambda (base t)
			(DELAY
			 (FV (t)
			     (cond ((empty-tree? t)
				    base)
				   (else
				    (let-tree ((v _ l r) t)
					      ((possiblyflip
						(lambda (l r)
						  (fold* (f v (fold* base r))
							 l)))
					       l r)))))))))
	(fold* base t)))))

(define tree:inorder-fold*
  (tree:inorder-fold_ identity))
(define tree:stream-inorder-fold*
  (tree:stream-inorder-fold_ identity))
;; suffix "-right" doesn't seem right, since the above was already right. Thus:
(define tree:inorder-fold-reverse*
  (tree:inorder-fold_ flip))
(define tree:stream-inorder-fold-reverse*
  (tree:stream-inorder-fold_ flip))
;; and the non-starred macros:
(define-define*s
  tree:inorder-fold
  tree:stream-inorder-fold
  tree:inorder-fold-reverse
  tree:stream-inorder-fold-reverse)

(define* (tree:members t)
  (tree:inorder-fold t cons '()))

(define* (tree:stream-members t)
  (tree:stream-inorder-fold t cons '()))

(define* (list->tree l)
  (fold (lambda (x t)
	  (tree:add t x))
	empty-tree
	l))

(define* (treesort l)
  (tree:members (list->tree l)))


;; fun fold_union t1 t2 = inorder_fold(reverse_add,t1,t2)
;; ^ inefficient


; split_lt and split_gt return a tree containing all those elements
; in the original tree which are less than (or greater than) the cut element
; v . 

; A tree is split by discarding all the unwanted elements and subtrees, and
; joining together all the wanted parts using concat3.

(define* (concat3 v l r)
  (cond ((empty-tree? l)
	 (tree:add r v))
	((empty-tree? r)
	 (tree:add l v))
	(else
	 (let*-tree (((v1 n1 l1 r1) l)
		     ((v2 n2 l2 r2)  r))
		    (if (< (* weight n1) n2)
			(T* v2 (concat3 v l l2) r2)
			(if (< (* weight n2) n1)
			    (T* v1 l1 (concat3 v r1 r))
			    (new-tree v l r)))))))

(define* (tree:lt t x)
  (let _lt ((t t))
    (cond ((empty-tree? t)
	   empty-tree)
	  (else
	   (let-tree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (_lt l))
		       ((gt) (concat3 v l (_lt r)))
		       ((eq) l)))))))

(define* (tree:gt t x)
  (let _gt ((t t))
    (cond ((empty-tree? t)
	   empty-tree)
	  (else
	   (let-tree ((v _ l r) t)
		     (match-cmp (cmp v x)
		       ((lt) (_gt r))
		       ((gt) (concat3 v (_gt l) r))
		       ((eq) r)))))))


(define tree:next-none (gensym 'no-next))
(define (tree:next-none? x)
  (eq? x tree:next-none))
;;^ ah btw might use empty-tree for that?
;; ah no. empty trees might be stored!

(define* (tree:next t x)
  ;; XXX: optimize this!!
  (let ((t* (tree:gt t x)))
    (if (empty-tree? t*)
	tree:next-none
	(tree:min t*))))




;; XXX: the paper has more efficient variants of union (O(n) instead of O(log n)) -- use them?
(define* (tree:union t1 t2)
  (let union ((t1 t1)
	      (t2 t2))
    (cond ((empty-tree? t1)
	   t2)
	  ((empty-tree? t2)
	   t1)
	  (else
	   (let-tree ((v _ l r) t2)
		     (let ((l* (tree:lt t1 v))
			   (r* (tree:gt t1 v)))
		       (concat3 v
				(union l* l)
				(union r* r))))))))


(IF #f
    (define* (concat t1 t2)
      (cond ((empty-tree? t2)
	     t1)
	    (else
	     (concat3 (tree:min t2)
		      t1
		      (tree:_delmin t2)))))

    ;;  "A slight improvement is to postpone the calls to min and delmin
    ;;  until the last possible moment. Then these functions operate on
    ;;  smaller trees. The rewritten concat then looks like concat3:"

    (define* (concat t1 t2)
      (cond ((empty-tree? t1)
	     t2)
	    ((empty-tree? t2)
	     t1)
	    (else
	     (let*-tree (((v1 n1 l1 r1) t1)
			 ((v2 n2 l2 r2) t2))
			(if (< (* weight n1) n2)
			    (T* v2 (concat t1 l2) r2)
			    (if (< (* weight n2) n1)
				(T* v1 l1 (concat r1 t2))
				(T* (tree:min t2) t1 (tree:_delmin t2)))))))))

(define* (tree:difference t1 t2)
  (cond ((empty-tree? t1)
	 empty-tree)
	((empty-tree? t2)
	 t1)
	(else
	 (let-tree ((v _ l r) t2)
		   (let ((l* (tree:lt t1 v))
			 (r* (tree:gt t1 v)))
		     (concat (tree:difference l* l)
			     (tree:difference r* r)))))))


(define* (tree:intersection t1 t2)
  (cond ((empty-tree? t1)
	 empty-tree)
	((empty-tree? t2)
	 empty-tree)
	(else
	 (let-tree ((v _ l r) t2)
		   (let ((l* (tree:lt t1 v))
			 (r* (tree:gt t1 v)))
		     (let ((i1 (tree:intersection l* l))
			   (i2 (tree:intersection r* r)))
		       (If (tree:member? t1 v)
			   (concat3 v i1 i2)
			   (concat i1 i2))))))))

(define* (trees:intersection-stream ts
				    #!optional (tail '()))
  (if (any empty-tree? ts)
      tail ;; delay?

      ;; v is the current member to be checked, always chosen from
      ;; the table at (car ts), so that only the rest of ts has to
      ;; be checked.
      (letrec
	  ((rec
	    (lambda (v ts)
	      (let ((current-t (car ts))
		    (other-ts (cdr ts)))
		(let rec2 ((v v))
		  (delay
		    (let ((next-rec2
			   (let ((v* (tree:next current-t v)))
			     (if (tree:next-none? v*)
				 tail
				 (rec2 v*)))))
		      ;; check the other 'legs'
		      (if (any (lambda (t)
				 (not (tree:member? t v)))
			       other-ts)
			  next-rec2
			  (cons v
				next-rec2)))))))))
	;; sort ts by tree size, as a best guess what would be useful
	;; of an intersection order as long as no dynamic reordering
	;; is being done.
	(let ((ts (sort ts (on (lambda (t)
				 ;; need lambda wrapper to
				 ;; pick up $treeparameter
				 ;; (tree:min is a macro)
				 (tree:size t))
			       <))))
	  (rec (tree:min (car ts))
	       ts)))))

(define* (tree->stream t tail)
  (trees:intersection-stream (list t) tail))

;; get the section of the tree between x1 (including?) and x2 (excluding?) -- excluding both, ok?
;;(define* (tree:section t x1 x2)
(define* (tree:between t x1 x2)
  (tree:intersection (tree:gt t x1)
		     (tree:lt t x2)))


(define* (tree:rank t x)
  (let rank ((t t))
    (cond ((empty-tree? t)
	   (raise 'not-found))
	  (else
	   (let-tree ((v n l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (rank l))
		       ((gt) (+ (rank r)
				(tree:size l)
				1))
		       ((eq) (tree:size l))))))))

(define* (tree:index t i)
  (cond ((empty-tree? t)
	 (raise 'not-found))
	(else
	 (let-tree ((v _ l r) t)
		   (let ((nl (tree:size l)))
		     (if (< i nl)
			 (tree:index l i)
			 (if (> i nl)
			     (tree:index r (- i nl 1))
			     v)))))))


;;<>\
