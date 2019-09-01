;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-typed
	 (list-util let-pair)
	 (cj-gambit-sys-0 @vector-ref))

(export table-test
	table-hash
	table-weak-keys
	table-weak-values
	table-init
	table
	table*
	table-keys
	table-values
	list->table-maybe-function
	table-delete!
	table-push!
	table-pop!
	#!optional
	_table
	)

;; For tests see table.scm


(define-typed (table-test #(table? t))
  ;; wow, `eq?` is turned into #f by Gambit; but the default procedure
  ;; is `equal?`, and passing #f to test: is not allowed.
  (or (@vector-ref t 2)
      eq?))

(define-typed (table-hash #(table? t))
  ;; ditto
  (or (@vector-ref t 3)
      eq?-hash))

;; 4 is the data vector

;; XX doesn't work
(define-typed (table-weak-keys #(table? t))
  (if (##gc-hash-table? (@vector-ref t 5))
      #f
      '?))

;; XX doesn't work
(define-typed (table-weak-values #(table? t))
  (if (##gc-hash-table? (@vector-ref t 5))
      #f
      '?))

(define-typed (table-init #(table? t))
  (@vector-ref t 6))

(define table:absent (table-init (list->table '())))

(define (_table options+pairs)
  (let lp ((opts '())
	   (l options+pairs))
    (define (t)
      (apply list->table l opts))
    (if (null? l)
	(t)
	(let-pair ((v l*) l)
		  (if (pair? v)
		      (t)
		      (let-pair ((w l**) l*)
				(lp (cons v (cons w opts))
				    l**)))))))

(define (table . options+pairs)
  (_table options+pairs))



(define (table* . options+pairs)
  (_table
   (let rec ((l options+pairs))
     (if (null? l)
	 l
	 (let-pair ((a l*) l)
		   (if (keyword? a)
		       (let-pair ((b l**) l*)
				 (cons a
				       (cons b
					     (rec l**))))
		       (let rec ((l l))
			 (if (null? l)
			     l
			     (let-pair ((a l*) l)
				       (let-pair ((b l**) l*)
						 (cons (cons a b)
						       (rec l**))))))))))))




(define (table-keys t)
  ;; XX more efficient?
  (map car (table->list t)))

(define (table-values t)
  ;; XX more efficient?
  (map cdr (table->list t)))

;; ^ sorted variants see table.scm


(define (list->table-maybe-function lis)
  (let ((t (list->table lis)))
    (lambda (k #!optional get-table?)
      (if get-table? t
	  (table-ref t k #f)))))


(define (table-delete! t key)
  (table-set! t key))


(define (table-push! t key val)
  (table-set! t key (cons val (table-ref t key '()))))

(define table:nothing (gensym 'table-nothing))
;; uh, allow access to table:nothing so that table-pop! can be called
;; with no alternate but a |clean?| value.

(define table-pop!
  (let ((nothing (box #f)))
    (lambda (t key
	  #!optional
	  (alternate table:nothing) 
	  (clean? #t))
      (let ((l (table-ref t key nothing)))
	(if (eq? l nothing)
	    (if (eq? alternate table:nothing)
		(error "table-pop!: key not found:" key)
		alternate)
	    (if (null? l)
		(if (eq? alternate table:nothing)
		    (error "table-pop!: empty list at key:" key)
		    alternate)
		(let-pair ((v r) l)
			  (if (and clean? (null? r))
			      (table-set! t key)
			      (table-set! t key r))
			  v)))))))
