;;; Copyright 2011, 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require dot-oo
	 symboltable-1
	 show ;; should it be possible to make things optional?!
	 test
	 cj-cmp
	 (dsssl sequential-pairs)
	 (cj-env keyword->symbol))

;; re-export all exports from symboltable-1, plus
(export (method symboltable.show)
	symboltable->sortedlist (method symboltable.sortedlist)
	symboltable-sortedkeys  (method symboltable.sortedkeys)
	symboltable-eqv? ;; method? is this really like eqv? 
	list->symbolcollection  (method list.symbolcollection)
	symboltable*

	;; and methods for symboltable-1 :
	(methods symboltable.length
		 symboltable.ref
		 symboltable.xref ;; XX bad, not same as usual .ref, right?
		 symboltable.maybe-ref
		 symboltable.contains?
		 ;;these have special behaviour so don't make methods:
		 ;; symboltable-update! 
		 ;; symboltable-set!
		 list.symboltable
		 symboltable.fold
		 symboltable.every
		 symboltable.list
		 symboltable.keys
		 ;; symboltable-update-all ?
		 ;; symboltable-add ?
		 symboltable.set
		 symboltable.remove ;; delete ?
		 symboltable.copy
		 )
	)


(include "cj-standarddeclares.scm")


(define (symboltable->sortedlist t)
  (cmp-sort (symboltable->list t) (on car symbol-cmp)))

(define. symboltable.sortedlist symboltable->sortedlist)


(define. (symboltable.show v)
  `(symboltable ,@(map .show (symboltable->sortedlist v))))

(define (symboltable-sortedkeys t #!optional (tail '()))
  (cmp-sort (symboltable-keys t tail) symbol-cmp))

(define. symboltable.sortedkeys symboltable-sortedkeys)


(TEST
 > (symboltable? empty-symboltable)
 #t
 > (symboltable? 'foo)
 #f)
;; ^ keep that test separate only so that failures will be shown
;; before passing buggy data to C (and risking endless loop) in the
;; test below.

(TEST
 > (symboltable-ref empty-symboltable 'ha 'not-found)
 not-found
 > (map (lambda (v)
	  (map (lambda (ref)
		 (with-exception-catcher
		  (lambda (x) (cond ((range-exception? x) 'range)
			       ((error-exception? x) 'error)
			       ((type-exception? x) 'type)
			       (else x)))
		  (lambda () (ref v 'ha 'not-found))))
	       (list symboltable-ref:scheme symboltable-ref:c)))
	(let ((l '(#() #(#f) #(#f #f) #(#f #f #f) #(0 #f #f) #(0 #f #f #f))))
	  `(foo
	    ,@l
	    ,@(map (lambda (v)
		     (list->vector (cons symboltable:tag
					 (vector->list v))))
		   l))))
 ;;with all-interpreted:
 ;; odd, now in compiled mode giving this as well?? Aha, some dependency mangle! XX
 ((error error)
  ;; no tag
  (error error)
  (error error)
  (error error)
  (error error)
  (error error)
  (error error)
  ;; with tag
  (error error)
  (error error)
  (error error)
  (error error)
  (not-found not-found)
  (not-found not-found))
 ;; ((type error)
 ;;  (range error)
 ;;  (range error)
 ;;  (range error)
 ;;  (not-found error)
 ;;  (not-found error)
 ;;  (not-found error)
 ;;  (range error)
 ;;  (range error)
 ;;  (not-found error)
 ;;  (not-found error)
 ;;  (not-found not-found)
 ;;  (not-found not-found))

 > (define l '(a b ha))
 > (define t
     (list->symboltable
      (map (lambda (pi)
	     (cons pi
		   (string-append "moo-"
				  (symboltable:key-name pi))))
	   l)))
 > t
 #((symboltable) 3 #f #f ha "moo-ha" b "moo-b" a "moo-a")
 > ((symboltable-of string?) t)
 #t
 > (symboltable-ref t 'ha 'not-found)
 "moo-ha"
 > (symboltable-ref t 'hu 'not-found)
 not-found
 > (symboltable-update! t 'ha (lambda (x) 1))
 > (symboltable-ref t 'ha 'not-found)
 1
 > (symboltable-update! t 'ha inc-function)
 > (symboltable-ref t 'ha 'not-found)
 2
 > (define t2 (symboltable-update t 'ha inc-function))
 > (symboltable-ref t 'ha 'not-found)
 2
 > (symboltable-ref t2 'ha 'not-found)
 3
 > (symboltable-update t 'hu inc-function (lambda () 'no))
 no
 > (define-values (t3 res)
     (symboltable-update* t2 'ha (lambda (v)
				   (values (inc v)
					   (/ v)))))
 > (symboltable-ref t3 'ha 'not-found)
 4
 > res
 1/3

 > t
 #((symboltable) 3 #f #f ha 2 b "moo-b" a "moo-a")
 > ((symboltable-of string?) t)
 #f
 > (define t2 (symboltable-add t 'c "moo-c"))
 > t2
 #((symboltable) 4 #f #f ha 2 #f #f #f #f #f #f c "moo-c" b "moo-b" a "moo-a")
 > ((symboltable-of (either number? string?)) t)
 #t
 > (symboltable-remove t2 'c)
 #((symboltable) 3 #f #f ha 2 b "moo-b" a "moo-a")
 > (symboltable-remove t2 'a)
 #((symboltable) 3 #f #f ha 2 c "moo-c" b "moo-b")
 > (symboltable-remove t2 'b)
 #((symboltable) 3 #f #f ha 2 c "moo-c" a "moo-a")
 ;; > (symboltable-remove (symboltable-remove t2 'a) 'b)
 ;; #((symboltable) 2 #f #f ha 2 c "moo-c" #f #f)
 > (define t22 (symboltable-remove t2 'a))
 > t22
 #((symboltable) 3 #f #f ha 2 c "moo-c" b "moo-b")
 > (symboltable-remove t22 'b)
 #((symboltable) 2 #f #f ha 2 c "moo-c" #f #f)

 > (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'c)
 #((symboltable) 1 #f #f ha 2)
 > (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'ha)
 #((symboltable) 1 #f #f c "moo-c")
 > (symboltable-remove (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'ha) 'c)
 #((symboltable) 0 #f #f)
 > (%try-error (symboltable-remove empty-symboltable 'c))
 #(error "key not in table:" c)

 > (symboltable-add t2 'd "moo-d")
 #((symboltable) 5 #f #f ha 2 #f #f #f #f d "moo-d" c "moo-c" b "moo-b" a "moo-a")

 > (%try-error (symboltable-add t 'b "moo-c"))
 #(error "key already in table:" b)
 > (symboltable-set t 'b "moo-c")
 #((symboltable) 3 #f #f ha 2 b "moo-c" a "moo-a")
 > (symboltable-set t 'c "c")
 #((symboltable) 4 #f #f ha 2 #f #f #f #f #f #f c "c" b "moo-b" a "moo-a")
 > (.show #)
 (symboltable (cons 'a "moo-a") (cons 'b "moo-b") (cons 'c "c") (cons 'ha 2))
 > (equal? (symboltable-add t 'c "c") (symboltable-set t 'c "c"))
 #t

 > (%try-error (symboltable-remove t 'nono))
 #(error "key not in table:" nono)

 ;; test removal of non-existing keys in the case where the table
 ;; would be resized:
 > (list->symboltable '())
 #((symboltable) 0 #f #f)
 > (list->symboltable '((a . 1)))
 #((symboltable) 1 #f #f a 1)
 ;; ^ not packing tighter because of too much scanning then, right?
 ;; (Or, case of endless wrap-around loop otherwise here?)
 > (list->symboltable '((a . 1) (b . 2)))
 #((symboltable) 2 #f #f #f #f b 2 a 1)
 > (symboltable-remove # 'a)
 #((symboltable) 1 b 2 #f #f)
 > (%try-error (symboltable-remove # 'a))
 #(error "key not in table:" a)
 ;; and not #(0 b 2)
 > (list->symboltable '((a . 1) (b . 2)))
 #((symboltable) 2 #f #f #f #f b 2 a 1)
 > (symboltable-remove # 'b)
 #((symboltable) 1 #f #f a 1)
 > (%try-error (symboltable-remove # 'b))
 #(error "key not in table:" b)
 ;; and not #(0 a 1)

 ;; list->symboltable-function
 > (def f (list->symboltable-function '((a . 1) (b . 2))))
 > (map f '(a b c))
 (1 2 #f)
 > (def f (list->symboltable-function '((a . 1) (b . 2)) -1))
 > (map f '(a b c))
 (1 2 -1)
 )


;; inline a local copy
(define symboltable? (symboltable?-lambda))

(define (symboltable-eqv? a b)
  (if (and (symboltable? a) (symboltable? b))
      (or (eq? a b)
	  (let ((len (symboltable-length a)))
	    (and (= len (symboltable-length b))
		 (let ((nothing (box b)))
		   ;; since we've checked that the length is the same,
		   ;; then if we never encounter a missing key from a in
		   ;; b, then there will also not be any missing key from
		   ;; b in a, hence we won't need to check for that (hence
		   ;; no record of our checking is necessary)
		   (symboltable:every?
		    a
		    (lambda (key val1)
		      ;; since `nothing` can't appear in a, we don't have
		      ;; to explicitely check for it here either.
		      (eq? val1 (symboltable-ref b key nothing))))))))
      (error "symboltable-eqv?: need two symboltables:" a b)))

(TEST
 > (def t1 (list->symboltable '((a . 1) (b . 3))))
 > (def t2 (list->symboltable '((b . 3) (a . 2) (c . 4))))
 > (symboltable-eqv? t1 t2)
 #f
 > (def t3 (symboltable-delete t2 'c))
 > (symboltable-eqv? t1 t3)
 #f
 > (def t4 (symboltable-set t3 'a 1))
 > (eq? t1 t4)
 #f
 > (symboltable-eqv? t1 t4)
 #t)


;; name? not really a new type, just a symboltable of #t values.
(define (list->symbolcollection l)
  (fold (lambda (v t)
	  (symboltable-set t v #t))
	empty-symboltable
	l))

(define. list.symbolcollection list->symbolcollection)

(TEST
 > (symboltable->sortedlist (list->symbolcollection '(a b c)))
 ((a . #t) (b . #t) (c . #t)))


;; Nicer 'mass' constructor?  (*Could* change this into a macro and
;; calculate the positions statically, i.e. turn into a |vector|
;; call. Well?)
(define (symboltable* . keywords+values)
  (list->symboltable (map (lambda (k+v)
			    (cons (keyword->symbol (car k+v))
				  (cdr k+v)))
			  (sequential-pairs keywords+values cons))))

(TEST
 > (.show (symboltable* b: 1 a: 2))
 (symboltable (cons 'a 2) (cons 'b 1)))


;; methods for symboltable-1 :

(define. symboltable.length symboltable-length)
(define. symboltable.ref symboltable-ref)
(define. symboltable.xref symboltable-xref)
(define. symboltable.maybe-ref symboltable-maybe-ref)
(define. symboltable.contains? symboltable-contains?)
;;these have special behaviour so don't make methods:
;; symboltable-update! 
;; symboltable-set!
(define. list.symboltable list->symboltable)
(define. (symboltable.fold s fn start)
  ;; heh confusing API here, good I wrap it...
  (symboltable:fold s start fn))
;; and unusual naming, dito:
(define. symboltable.every symboltable:every?)
(define. symboltable.list symboltable->list)
(define. symboltable.keys symboltable-keys)
;; symboltable-update-all ?
;; symboltable-add ?
(define. symboltable.set symboltable-set)
(define. symboltable.remove symboltable-remove) ;; delete ?
(define. symboltable.copy symboltable-copy)
