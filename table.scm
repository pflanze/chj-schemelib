;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 show
	 test
	 cj-cmp
	 list-util
	 (srfi-1 any))

(export table _table
	table*
	;; accessors:
	(method table.test
		table.hash
		;; XX warning, .weak-keys and .weak-values not working
		table.init
		table.show
		table.ref 
		table.set! 
		table.delete! table-delete!
		table.push!)
	
	;; utilities:
	(method table.list
		table.keys
		table.sorted-keys
		table.values
		table.sorted-values)
	list.table-maybe-function)



;; finally provide a nicer interface to creating tables

(define. (table.test #(table? t))
  ;; wow, `eq?` is turned into #f by Gambit; but the default procedure
  ;; is `equal?`, and passing #f to test: is not allowed.
  (or (##vector-ref t 2)
      eq?))

(define. (table.hash #(table? t))
  ;; dito
  (or (##vector-ref t 3)
      eq?-hash))

;; 4 is the data vector

;; XX doesn't work
(define. (table.weak-keys #(table? t))
  (if (##gc-hash-table? (##vector-ref t 5))
      #f
      '?))

;; XX doesn't work
(define. (table.weak-values #(table? t))
  (if (##gc-hash-table? (##vector-ref t 5))
      #f
      '?))

(define. (table.init #(table? t))
  (##vector-ref t 6))

(define table:absent (table.init (list->table '())))

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


(define. (table.show t)
  (define (get key op dflts)
    (let ((v (op t)))
      (if (any (lambda (dflt _) ;; _ is to prevent the absent value from
			   ;; interfering with function arg checks .
		 (eq? dflt v))
	       dflts (make-list (length dflts) #t))
	  '()
	  `(,key ,(.show v)))))
  `(table
    ,@(get test: table.test (list ##equal? equal?))
    ,@(get hash: table.hash
	   (let ((t (table.test t)))
	     (cond ((or (eq? t ##eq?)
			(eq? t eq?))
		    (list ##eq?-hash eq?-hash))
		   ((or (eq? t ##equal?)
			(eq? t equal?))
		    (list ##equal?-hash equal?-hash))
		   ((or (eq? t =)
			(eq? t ##=))
		    ;; Hm, does that mean it's slow? No, still hashes?
		    (list ##generic-hash))
		   (else
		    (list ##equal?-hash equal?-hash)))))
    ,@(get weak-keys: table.weak-keys (list #f))
    ,@(get weak-values: table.weak-values (list #f))
    ,@(get init: table.init (list table:absent))
    ,@(map .show (cmp-sort (table->list t) (on car generic-cmp)))))

(TEST
 > (.show (list->table '((a . 1) (b . 2))))
 (table (cons 'a 1) (cons 'b 2))
 > (.show (list->table '((a . 1) (b . 2)) test: eq?))
 (table test: eq? (cons 'a 1) (cons 'b 2))
 > (.show (list->table '((a . 1) (b . 2)) test: equal?))
 (table (cons 'a 1) (cons 'b 2))
 > (.show (list->table '((2 . a) (1 . b)) test: =))
 (table test: = (cons 1 'b) (cons 2 'a))

 ;;XX hm these don't work
 ;; > (.show (list->table '(("a" . 1) ("b" . 2)) weak-keys: #t))
 ;; (table weak-keys: #t (cons "a" 1) (cons "b" 2))
 ;; > (.show (list->table '(("a" . 1) ("b" . 2)) weak-values: #t))
 ;; (table weak-values: #t (cons "a" 1) (cons "b" 2))
 ;; > (.show (list->table '(("a" . 1) ("b" . 2)) weak-keys: #t weak-values: #t))
 ;; (table weak-keys: #t weak-values: #t (cons "a" 1) (cons "b" 2))

 > (.show (list->table '((a . 1) (b . 2)) init: 123))
 (table init: 123 (cons 'a 1) (cons 'b 2))
 )


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


(TEST
 > (.show (table* init: 123))
 (table weak-keys: '? weak-values: '? init: 123)
 > (.show (table* "b" 2 "a" 1))
 (table (cons "a" 1) (cons "b" 2))
 > (.show (table* init: 123 "b" 2 "a" 1))
 (table init: 123 (cons "a" 1) (cons "b" 2)))


(define. table.list table->list)

(define. (table.keys t)
  ;; XX more efficient?
  (map car (table->list t)))

(define. (table.sorted-keys t)
  (cmp-sort (table.keys t) generic-cmp))

(define. (table.values t)
  ;; XX more efficient?
  (map cdr (table->list t)))

(define. (table.sorted-values t #!optional (cmp generic-cmp))
  (cmp-sort (table.values t) cmp))


(define (list.table-maybe-function lis)
  (let ((t (list->table lis)))
    (lambda (k #!optional get-table?)
      (if get-table? t
	  (table-ref t k #f)))))



(define. table.ref table-ref)
(define. table.set! table-set!)
(define. (table.delete! t key)
  (table-set! t key))

(define table-delete! table.delete!)

(define. (table.push! t key val)
  (table-set! t key (cons val (table-ref t key '()))))

(define table:nothing (gensym 'table-nothing))
;; uh, allow access to table:nothing so that table.pop! can be called
;; with no alternate but a |clean?| value.

(define. table.pop!
  (let ((nothing (box #f)))
    (lambda (t key
	  #!optional
	  (alternate table:nothing) 
	  (clean? #t))
      (let ((l (table-ref t key nothing)))
	(if (eq? l nothing)
	    (if (eq? alternate table:nothing)
		(error "table.pop!: key not found:" key)
		alternate)
	    (if (null? l)
		(if (eq? alternate table:nothing)
		    (error "table.pop!: empty list at key:" key)
		    alternate)
		(let-pair ((v r) l)
			  (if (and clean? (null? r))
			      (table-set! t key)
			      (table-set! t key r))
			  v)))))))

(TEST
 > (define t (make-table))
 > (.push! t "a" 1)
 > (.push! t "b" 1)
 > (.push! t "a" 2)
 > (.ref t "a")
 (2 1)
 > (.pop! t "a")
 2
 > (.ref t "a")
 (1)
 > (%try (.pop! t "y"))
 (exception text: "table.pop!: key not found: \"y\"\n")
 > (.pop! t "a" 'n)
 1
 > (.pop! t "a" 'n)
 n
 > (.list t)
 (("b" 1)))

