;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>
;;; based on a text (c) University of Southampton (see below)

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require cj-struct
	 (cj-env IF compile-time)
	 define-strict-and-lazy
	 debuggable-promise)

(export empty-wbtree
	empty-wbtree?
	wbtree? ;; CAREFUL, dangerous in optimized variant!
	wbtreeparameter*
	wbtreeparameter-equal? ;; why export it, should it be hidden?
			       ;; But why is wbtreeparameter*
			       ;; exported?

	(struct wbtree) ;; XX but with _ prefix

	;; XX both as macros as well as functions:
	wbtree:size
	wbtree:member?
	wbtree:maybe-ref
	wbtree:maybe-ref&rank
	wbtree:min
	wbtree:max
	wbtree:set
	wbtree:add
	wbtree:delete
	wbtree:inorder-fold
	wbtree:stream-inorder-fold
	wbtree:inorder-fold-reverse
	wbtree:stream-inorder-fold-reverse
	wbtree:members
	wbtree:stream-members
	list->wbtree
	wbtreesort
	wbtree:lt
	wbtree:gt
	wbtree:next
	wbtree:union
	wbtree:difference
	wbtree:intersection
	wbtrees:intersection-stream
	wbtree->stream
	wbtree:between
	wbtree:rank
	wbtree:index
	
	#!optional
	make-wbtree
	new-wbtree ;; vs make-wbtree ?
	;; XX both as macros as well as functions:
	wbtree-element
	wbtree-size
	wbtree-left
	wbtree-right)


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)

;;(not safe)


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



(define-struct wbtreeparameter
  constructor-name: wbtreeparameter
  cmp
  element?)


;; XXX: shouldn't we simply drop element? from wbtreeparameter
;; instead?
(define (not-_wbtree? v) (not (_wbtree? v)))
(define (wbtreeparameter* cmp)
  (wbtreeparameter cmp not-_wbtree?))

;; (also a ~general thing ? Almost 'on x cmp'? 'on x eq?') 
(define (wbtreeparameter-equal? a b)
  (let-wbtreeparameter ((a-cmp a-element?) a)
		       (let-wbtreeparameter ((b-cmp b-element?) b)
					    (and (eq? a-cmp b-cmp)
						 (eq? a-element? b-element?)))))


(define (wbtreeparameter:wrap-in tp type? access)
  (let-wbtreeparameter
   ((cmp _) tp) ;; ignoring original |element?| check (not going to nest it)
   (wbtreeparameter (on access cmp)
		    type?)))

;; thread param through functions: using macros to reduce the amount
;; of code that needs to be changed:

;; using |$wbtreeparameter| as parameter variable name everywhere.

(define-macro* (define* name+vars . body)
  (match* name+vars
	  ((name . vars)
	   (let ((name* (symbol-append (source-code name) "*")))
	     ;; at the same time, define a macro for the nonstarred name
	     `(begin
		(define-macro* (,name . argexprs)
		  (cons ',name*
			(cons '$wbtreeparameter
			      argexprs)))
		(define (,name* ,@(cons '$wbtreeparameter vars))
		  ;; locally define |cmp| and |element?|:
		  (let-wbtreeparameter
		   ((cmp element?) $wbtreeparameter)
		   ,@body)))))))

;; to turn a function that has manually been defined with name ending
;; in * and taking $wbtreeparameter as first argument into a macro:
(define-macro* (define-define*s . names)
  `(begin
     ,@(map (lambda (name)
	      (let ((name* (symbol-append (source-code name) "*")))
		`(define-macro* (,name . argexprs)
		   (cons ',name*
			 (cons '$wbtreeparameter
			       argexprs)))))
	    (source-code names))))



(define weight 2);;XXX how parametrize this too


(define empty-wbtree 'empty-wbtree) ;; XXX is this safe or not?
(define (empty-wbtree? x)
  (eq? x empty-wbtree))

(both-times
 (define use-wbtrees-as-leafs? #f))

(IF use-wbtrees-as-leafs?

    (begin ;; original variant, i.e. 'type-safe'
      (define-struct wbtree
	predicate-name: _wbtree?
	let-fallback?: #t

	element	;; element
	size	;; int
	left	;; wbtree
	right	;; wbtree
	)
      (define (wbtree? v)
	(or (_wbtree? v)
	    (empty-wbtree? v)))

      ;; XXX todo: wbtree-left / wbtree-left* and probably more are
      ;; missing here now, fix that some time.
      )

    (begin ;; optimized variant
      (define-struct wbtree
	;; constructor-name: _make-wbtree
	prefix: "_"
	generic-accessor-prefix: "wbtree-"
	let*-name: let*-wbtree
	let-fallback?: #t

	element	;; element
	size	;; int
	left	;; wbtree
	right	;; wbtree
	)
      ;; can't type-check in optimized variant! DANGER!
      (define (wbtree? v)
	#t)

      ;; dynamic dispatch, to save wrapping the leaf elements in wbtrees:
      (define (make-wbtree e s l r)
	(if (and (eq? l empty-wbtree)
		 (eq? r empty-wbtree))
	    e
	    (_make-wbtree e s l r)))

					; (define (_access accessor)
					;   (lambda (v)
					;     (if (element? v)
					; 	ah sh alle verschieden)))

      ;; (hm no need for runtime macro here hm. ?. localized would suffice)
      (define-macro* (_access access-element access-wbtree)
	`(lambda (v)
	   (if (element? v)
	       (,access-element v)
	       (,access-wbtree v))))

      (define* (wbtree-element v)
	((_access identity _wbtree-element) v))
      (define* (wbtree-size v)
	((_access (lambda (v)
		    1)
		  _wbtree-size) v))
      (define* (wbtree-left v)
	((_access (lambda (v)
		    empty-wbtree)
		  _wbtree-left) v))
      (define* (wbtree-right v)
	((_access (lambda (v)
		    empty-wbtree)
		  _wbtree-right) v))
      ;; / dynamic dispatch
      ))


(define* (wbtree:size t)
  (cond ((empty-wbtree? t)
	 0)
	(else
	 (wbtree-size t))))

;; fun N(v,l,r) = T(v, 1 + size l + size r, l, r)
(define* (new-wbtree v l r)
  (make-wbtree v
	     (+ 1
		(wbtree:size l)
		(wbtree:size r))
	     l
	     r))

(IF #f
    (begin ;; C variant, see wbwbtree-C
      )
    (begin ;; Scheme variant
      (define* (wbtree:member? t x)
	(let member? ((t t))
	  (cond ((empty-wbtree? t)
		 #f)
		(else
		 (let ((v (wbtree-element t)))
		   (match-cmp (cmp x v)
		     ((lt) (member? (wbtree-left t)))
		     ((gt) (member? (wbtree-right t)))
		     ((eq) #t)))))))

      ;; and with 1 *single* change:
      (define* (wbtree:maybe-ref t x)
	(let member? ((t t))
	  (cond ((empty-wbtree? t)
		 #f)
		(else
		 (let ((v (wbtree-element t)))
		   (match-cmp (cmp x v)
		     ((lt) (member? (wbtree-left t)))
		     ((gt) (member? (wbtree-right t)))
		     ((eq) v)))))))
      ))

;; variant that also returns the position:
;; (they call it rank, not position nor index; wbtree:index goes from the index to the element)
(define* (wbtree:maybe-ref&rank t x)
  (let member? ((t t)
		(nleft 0))
    (cond ((empty-wbtree? t)
	   #f)
	  (else
	   (let-wbtree ((v _ l _) t)
		     (let ((nleft* (lambda ()
				     (+ nleft
					(wbtree:size l)))))
		       (match-cmp
			(cmp x v)
			((lt) (member? l
				       nleft))
			((gt) (member? (wbtree-right t)
				       (+ (nleft*) 1)))
			((eq) (cons v
				    (nleft*))))))))))


(define* (_wbtree-minmax minmax _wbtree-left* t)
  (let rec ((t t))
    (cond ((empty-wbtree? t)
	   (if minmax
	       (error (string-append
		       "can't get " minmax " from empty wbtree"))
	       #f))
	  (else
	   (let ((l (_wbtree-left* $wbtreeparameter t)))
	     (cond ((empty-wbtree? l)
		    (wbtree-element t))
		   (else
		    (rec l))))))))

(define* (wbtree:min t)
  (_wbtree-minmax "min" wbtree-left* t))

(define* (wbtree:max t)
  (_wbtree-minmax "max" wbtree-right* t))

(define* (wbtree:maybe-min t)
  (_wbtree-minmax #f wbtree-left* t))

(define* (wbtree:maybe-max t)
  (_wbtree-minmax #f wbtree-right* t))


; fun single_L (a,x,T(b,_,y,z))         = N(b,N(a,x,y),z)
; fun single_R (b,T(a,_,x,y),z)         = N(a,x,N(b,y,z))

(define* (single-l a x t)
  (let-wbtree ((b _ y z) t)
	    (new-wbtree b
		      (new-wbtree a
				x
				y)
		      z)))

(define* (single-r a t x) ;; z
  ;; ^ -- transp
  (let-wbtree ((b _ y z) t)
	    ;; (b _ x y)
	    (new-wbtree b
		      y	;;hrm nid z
		      ;; -- transp
		      (new-wbtree a
				z ;; hrm nid y  also diese auch noch vertauschn.
				;; --auch noch transp
				x))))


; fun double_L (a,x,T(c,_,T(b,_,y1,y2),z)) =
;                               N(b,N(a,x,y1),N(c,y2,z))

(define* (double-l a x t1)
  (let*-wbtree (((c _ t2 z) t1)
	      ((b _ y1 y2) t2))
	     (new-wbtree b
		       (new-wbtree a x y1)
		       (new-wbtree c y2 z))))

; fun double_R (c,T(a,_,x,T(b,_,y1,y2)),z) =
;                               N(b,N(a,x,y1),N(c,y2,z))

(define* (double-r c t1 z)
  (let*-wbtree (((a _ x t2) t1)
	      ((b _ y1 y2) t2))
	     (new-wbtree b
		       (new-wbtree a x y1)
		       (new-wbtree c y2 z))))


(define* (T* v l r)
  (let ((ln (wbtree:size l))
	(rn (wbtree:size r)))
    (if (< (+ ln rn) 2)
	(new-wbtree v l r)
	(if (> rn (* weight ln)) ;;XX integer ops?
	    ;; right is too big
	    (let* ((rl (wbtree-left r))
		   (rr (wbtree-right r))
		   (rln (wbtree:size rl))
		   (rrn (wbtree:size rr)))
	      (if (< rln rrn)
		  (single-l v l r)
		  (double-l v l r)))
	    (if (> ln (* weight rn))
		;; left is too big
		(let* ((ll (wbtree-left l))
		       (lr (wbtree-right l))
		       (lln (wbtree:size ll))
		       (lrn (wbtree:size lr)))
		  (if (< lrn lln)
		      (single-r v l r)
		      (double-r v l r)))
		(new-wbtree v l r))))))


;; wbtree:set: setting an element that already exists with regards to
;; cmp replaces the previous one with the new one.
(define* (wbtree:set t x)
  (let add ((t t))
    (cond ((empty-wbtree? t)
	   (make-wbtree x 1 empty-wbtree empty-wbtree))
	  (else
	   (let-wbtree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (T* v (add l) r))
		       ((gt) (T* v l (add r)))
		       ((eq)
			;; key already there, but replace with new
			;; element, ok? (i.e. don't just return t)
			(new-wbtree x l r))))))))


(define-struct wbtree-duplicate-exception
  constructor-name: wbtree-duplicate-exception
  old-element
  new-element)

;; wbtree:add: adding an element that's already there with regards to
;; cmp is an error and raises a wbtree-duplicate-exception
(define* (wbtree:add t x)
  (let add ((t t))
    (cond ((empty-wbtree? t)
	   (make-wbtree x 1 empty-wbtree empty-wbtree))
	  (else
	   (let-wbtree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (T* v (add l) r))
		       ((gt) (T* v l (add r)))
		       ((eq)
			(raise (wbtree-duplicate-exception v x)))))))))


; (define (wbtree:_delete* l r)
;   (cond ((empty-wbtree? l)
; 	 r)
; 	((empty-wbtree? r)
; 	 l)
; 	(else
; 	 (let ((min-elt (wbtree:min r)))
; 	   (T* min-elt l (wbtree:delete r min-elt))))))

;;^ *not efficient*

(define* (wbtree:_delmin t)
  (cond ((empty-wbtree? (wbtree-left t))
	 (wbtree-right t))
	(else
	 (let-wbtree ((v _ l r) t)
		   (T* v (wbtree:_delmin l) r)))))

(define* (wbtree:delete t x)
  
  (define (wbtree:_delete* l r)
    (cond ((empty-wbtree? l)
	   r)
	  ((empty-wbtree? r)
	   l)
	  (else
	   (T* (wbtree:min r) l (wbtree:_delmin r)))))

  (let delete ((t t))
    (cond ((empty-wbtree? t)
	   ;; XXX die instead?
	   empty-wbtree)
	  (else
	   (let-wbtree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (T* v (delete l) r))
		       ((gt) (T* v l (delete r)))
		       ((eq) (wbtree:_delete* l r))))))))


;;(define (wbtree:inorder-fold f base wbtree)

(define-strict-and-lazy
  wbtree:inorder-fold_
  wbtree:stream-inorder-fold_

  (lambda (possiblyflip)
    (lambda ($wbtreeparameter t f base)
      (letrec ((fold* (lambda (base t)
			(DELAY
			 (FV (t)
			     (cond ((empty-wbtree? t)
				    base)
				   (else
				    (let-wbtree ((v _ l r) t)
					      ((possiblyflip
						(lambda (l r)
						  (fold* (f v (fold* base r))
							 l)))
					       l r)))))))))
	(fold* base t)))))

(define wbtree:inorder-fold*
  (wbtree:inorder-fold_ identity))
(define wbtree:stream-inorder-fold*
  (wbtree:stream-inorder-fold_ identity))
;; suffix "-right" doesn't seem right, since the above was already right. Thus:
(define wbtree:inorder-fold-reverse*
  (wbtree:inorder-fold_ flip-function))
(define wbtree:stream-inorder-fold-reverse*
  (wbtree:stream-inorder-fold_ flip-function))
;; and the non-starred macros:
(define-define*s
  wbtree:inorder-fold
  wbtree:stream-inorder-fold
  wbtree:inorder-fold-reverse
  wbtree:stream-inorder-fold-reverse)

(define* (wbtree:members t)
  (wbtree:inorder-fold t cons '()))

(define* (wbtree:stream-members t)
  (wbtree:stream-inorder-fold t cons '()))

(define* (list->wbtree l #!optional ignore-duplicates?)
  (fold (if ignore-duplicates?
	    (lambda (x t)
	      (wbtree:set t x))
	    (lambda (x t)
	      (wbtree:add t x)))
	empty-wbtree
	l))

(define* (wbtreesort l #!optional ignore-duplicates?)
  (wbtree:members (list->wbtree l ignore-duplicates?)))


;; fun fold_union t1 t2 = inorder_fold(reverse_add,t1,t2)
;; ^ inefficient


; split_lt and split_gt return a wbtree containing all those elements
; in the original wbtree which are less than (or greater than) the cut element
; v . 

; A wbtree is split by discarding all the unwanted elements and subwbtrees, and
; joining together all the wanted parts using concat3.

(define* (concat3 v l r)
  (cond ((empty-wbtree? l)
	 (wbtree:set r v))
	((empty-wbtree? r)
	 (wbtree:set l v))
	(else
	 (let*-wbtree (((v1 n1 l1 r1) l)
		     ((v2 n2 l2 r2)  r))
		    (if (< (* weight n1) n2)
			(T* v2 (concat3 v l l2) r2)
			(if (< (* weight n2) n1)
			    (T* v1 l1 (concat3 v r1 r))
			    (new-wbtree v l r)))))))

(define* (wbtree:lt t x)
  (let _lt ((t t))
    (cond ((empty-wbtree? t)
	   empty-wbtree)
	  (else
	   (let-wbtree ((v _ l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (_lt l))
		       ((gt) (concat3 v l (_lt r)))
		       ((eq) l)))))))

(define* (wbtree:gt t x)
  (let _gt ((t t))
    (cond ((empty-wbtree? t)
	   empty-wbtree)
	  (else
	   (let-wbtree ((v _ l r) t)
		     (match-cmp (cmp v x)
		       ((lt) (_gt r))
		       ((gt) (concat3 v (_gt l) r))
		       ((eq) r)))))))


(define wbtree:next-none (gensym 'no-next))
(define (wbtree:next-none? x)
  (eq? x wbtree:next-none))
;;^ ah btw might use empty-wbtree for that?
;; ah no. empty wbtrees might be stored!

(define* (wbtree:next t x)
  ;; XXX: optimize this!!
  (let ((t* (wbtree:gt t x)))
    (if (empty-wbtree? t*)
	wbtree:next-none
	(wbtree:min t*))))




;; XXX: the paper has more efficient variants of union (O(n) instead of O(log n)) -- use them?
(define* (wbtree:union t1 t2)
  (let union ((t1 t1)
	      (t2 t2))
    (cond ((empty-wbtree? t1)
	   t2)
	  ((empty-wbtree? t2)
	   t1)
	  (else
	   (let-wbtree ((v _ l r) t2)
		     (let ((l* (wbtree:lt t1 v))
			   (r* (wbtree:gt t1 v)))
		       (concat3 v
				(union l* l)
				(union r* r))))))))


(IF #f
    (define* (concat t1 t2)
      (cond ((empty-wbtree? t2)
	     t1)
	    (else
	     (concat3 (wbtree:min t2)
		      t1
		      (wbtree:_delmin t2)))))

    ;;  "A slight improvement is to postpone the calls to min and delmin
    ;;  until the last possible moment. Then these functions operate on
    ;;  smaller wbtrees. The rewritten concat then looks like concat3:"

    (define* (concat t1 t2)
      (cond ((empty-wbtree? t1)
	     t2)
	    ((empty-wbtree? t2)
	     t1)
	    (else
	     (let*-wbtree (((v1 n1 l1 r1) t1)
			 ((v2 n2 l2 r2) t2))
			(if (< (* weight n1) n2)
			    (T* v2 (concat t1 l2) r2)
			    (if (< (* weight n2) n1)
				(T* v1 l1 (concat r1 t2))
				(T* (wbtree:min t2) t1 (wbtree:_delmin t2)))))))))

(define* (wbtree:difference t1 t2)
  (cond ((empty-wbtree? t1)
	 empty-wbtree)
	((empty-wbtree? t2)
	 t1)
	(else
	 (let-wbtree ((v _ l r) t2)
		   (let ((l* (wbtree:lt t1 v))
			 (r* (wbtree:gt t1 v)))
		     (concat (wbtree:difference l* l)
			     (wbtree:difference r* r)))))))


(define* (wbtree:intersection t1 t2)
  (cond ((empty-wbtree? t1)
	 empty-wbtree)
	((empty-wbtree? t2)
	 empty-wbtree)
	(else
	 (let-wbtree ((v _ l r) t2)
		   (let ((l* (wbtree:lt t1 v))
			 (r* (wbtree:gt t1 v)))
		     (let ((i1 (wbtree:intersection l* l))
			   (i2 (wbtree:intersection r* r)))
		       (If (wbtree:member? t1 v)
			   (concat3 v i1 i2)
			   (concat i1 i2))))))))

(define* (wbtrees:intersection-stream ts
				    #!optional (tail '()))
  (if (any empty-wbtree? ts)
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
			   (let ((v* (wbtree:next current-t v)))
			     (if (wbtree:next-none? v*)
				 tail
				 (rec2 v*)))))
		      ;; check the other 'legs'
		      (if (any (lambda (t)
				 (not (wbtree:member? t v)))
			       other-ts)
			  next-rec2
			  (cons v
				next-rec2)))))))))
	;; sort ts by wbtree size, as a best guess what would be useful
	;; of an intersection order as long as no dynamic reordering
	;; is being done.
	(let ((ts (sort ts (on (lambda (t)
				 ;; need lambda wrapper to
				 ;; pick up $wbtreeparameter
				 ;; (wbtree:min is a macro)
				 (wbtree:size t))
			       <))))
	  (rec (wbtree:min (car ts))
	       ts)))))

(define* (wbtree->stream t tail)
  (wbtrees:intersection-stream (list t) tail))

;; get the section of the wbtree between x1 (including?) and x2 (excluding?) -- excluding both, ok?
;;(define* (wbtree:section t x1 x2)
(define* (wbtree:between t x1 x2)
  (wbtree:intersection (wbtree:gt t x1)
		     (wbtree:lt t x2)))


(define* (wbtree:rank t x)
  (let rank ((t t))
    (cond ((empty-wbtree? t)
	   (raise 'not-found))
	  (else
	   (let-wbtree ((v n l r) t)
		     (match-cmp (cmp x v)
		       ((lt) (rank l))
		       ((gt) (+ (rank r)
				(wbtree:size l)
				1))
		       ((eq) (wbtree:size l))))))))

(define* (wbtree:index t i)
  (cond ((empty-wbtree? t)
	 (raise 'not-found))
	(else
	 (let-wbtree ((v _ l r) t)
		   (let ((nl (wbtree:size l)))
		     (if (< i nl)
			 (wbtree:index l i)
			 (if (> i nl)
			     (wbtree:index r (- i nl 1))
			     v)))))))


