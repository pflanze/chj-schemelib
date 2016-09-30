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

(export (method table.test
		table.hash
		;; XX warning, .weak-keys and .weak-values not working
		table.init
		table.show)
	table)


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

(define (table . options+pairs)
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
				(lp l**
				    (cons v (cons w opts)))))))))

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


