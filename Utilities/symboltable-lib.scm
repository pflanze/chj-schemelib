;;; Copyright 2011 by Christian Jaeger <chrjae@gmail.com>

;;; This file is part of GIT System.
;;;
;;;    GIT System is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU Lesser General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    GIT System is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU Lesser General Public License for more details.
;;;
;;;    You should have received a copy of the GNU Lesser General Public License
;;;    along with GIT System.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;;; Utilities for / built on symboltables
;;;

;; "Inverse symbol-vectors":

(define (positionallist->symboltable l)
  (list->symboltable
   (map cons
	l
	(iota (length l)))))

(define (vector->symboltable vec)
  (positionallist->symboltable (vector->list vec)))


(TEST
 > (define t (vector->symboltable '#(a b c)))
 > (cmp-sort (symboltable->list t) (on cdr number-cmp))
 ((a . 0) (b . 1) (c . 2))
 ;; being lazy, reusing the above test data for symboltable-update-all:
 > (cmp-sort (symboltable->list
	      (symboltable-update-all t
				      (lambda (k v)
					(dec v))))
	     (on cdr number-cmp))
 ((a . -1) (b . 0) (c . 1))
 )


;; Caches for symbolic keys

(define symboltable:nothing (gensym))

(define symboltable:caching/1-first-time-count 0)

(define (symboltable:caching/1 fn)
  (let ((cache empty-symboltable))
    (lambda (k)
      (let ((cache* cache))
	(let ((v (symboltable-ref cache* k symboltable:nothing)))
	  (if (eq? v symboltable:nothing)
	      (begin
		(inc! symboltable:caching/1-first-time-count)
		(let ((v (fn k)))
		  (set! cache
			(symboltable-add cache* k v))
		  v))
	      v))))))

;; this is rather just a workaround for the missing symboltable-set,
;; offering the same api as symboltable-add (and certainly not
;; working in the most efficient manner):
(define (symboltable-replace t k v)
  (symboltable-update t k (lambda (_)
			    v)))

(define (symboltable:caching/2 fn)
  (let ((cache1 empty-symboltable))
    (lambda (k1 k2)
      (let* ((cache1* cache1)
	     (set
	      (lambda (symboltable-set cache2*)
		(let ((v (fn k1 k2)))
		  (inc! symboltable:caching/1-first-time-count)
		  (set! cache1
			(symboltable-set cache1*
					 k1
					 (symboltable-add cache2*
							  k2
							  v)))
		  v))))
	(cond ((symboltable-ref cache1* k1 #f)
	       => (lambda (cache2*)
		    (let ((v (symboltable-ref cache2* k2 symboltable:nothing)))
		      (if (eq? v symboltable:nothing)
			  (set symboltable-replace cache2*)
			  v))))
	      (else
	       (set symboltable-add empty-symboltable)))))))

(TEST
 > (define count 0)
 > (define t (symboltable:caching/2
	      (lambda (x y)
		(inc! count)
		(symbol-append x y))))
 > (t 'a 'b)
 ab
 > count
 1
 > (t 'a 'b)
 ab
 > count
 1
 > (t 'c 'b)
 cb
 > count
 2
 > (t 'c 'c)
 cc
 > count
 3
 > (t 'c 'b)
 cb
 > count
 3
 )



;; Symbol hierarchy

(define (maybe-_-symbol stringpart)
  (lambda (splitchar)
    (symboltable:caching/1
     (lambda (sym)
       (let* ((str (symbol->string sym))
	      (len (string-length str))
	      (parent (let lp ((i (dec len)))
			(if (negative? i)
			    #f ;; nil.  !
			    (if (char=? (string-ref str i) splitchar)
				(string->symbol
				 (stringpart str i len))
				(lp (dec i)))))))
	 parent)))))

(define maybe-parent-symbol
  (maybe-_-symbol (lambda (str mid end)
		    (substring str 0 mid))))

(define maybe-leaf-symbol
  (maybe-_-symbol (lambda (str mid end)
		    (substring str (inc mid) end))))

(define maybe-parent-.-symbol (maybe-parent-symbol #\.))
(define maybe-leaf-.-symbol (maybe-leaf-symbol #\.))

(TEST
 > (define symboltable:caching/1-first-time-count 0)
 > (define maybe-parent-.-symbol (maybe-parent-symbol #\.))
 > symboltable:caching/1-first-time-count
 0
 > (maybe-parent-.-symbol 'Foo)
 #f
 > symboltable:caching/1-first-time-count
 1
 > (maybe-parent-.-symbol 'Foo)
 #f
 > symboltable:caching/1-first-time-count
 1
 > (maybe-parent-.-symbol 'Foo.bar)
 Foo
 > symboltable:caching/1-first-time-count
 2
 > (maybe-parent-.-symbol 'Foo.bar)
 Foo
 > symboltable:caching/1-first-time-count
 2
 > (maybe-parent-.-symbol '.bar)
 ||
 > symboltable:caching/1-first-time-count
 3
 ;; and maybe-leaf-symbol:
 > (define maybe-leaf-%-symbol (maybe-leaf-symbol #\%))
 > (maybe-leaf-%-symbol 'Foo.bar%baz)
 baz
 > symboltable:caching/1-first-time-count
 4
 > (maybe-leaf-%-symbol 'Foo.bar%baz)
 baz
 > symboltable:caching/1-first-time-count
 4
 > (maybe-leaf-%-symbol 'Foo.bar%baz%buzz)
 buzz
 > symboltable:caching/1-first-time-count
 5
 > (maybe-leaf-%-symbol 'Foo.bar%baz%buzz)
 buzz
 > symboltable:caching/1-first-time-count
 5
 )


