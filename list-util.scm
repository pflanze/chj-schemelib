;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 srfi-1
	 (cj-env-1 dec inc identity)
	 (string-util-1 string-split)
	 (improper-list improper-length)
	 )


;; destructuring syntax

(define-macro* (let-pair bindform . body)
  (match-list*
   bindform
   ((vars expr)
    (match-list*
     vars
     ((a r)
      (let ((V (gensym)))
	`(let ((,V ,expr))
	   (let ((,a (car ,V))
		 (,r (cdr ,V)))
	     ,@body))))))))

(define-macro* (let-pair* bindforms . body)
  (let rec ((bindforms bindforms))
    (match-list*
     bindforms
     ((bindform . bindforms*)
      `(let-pair ,bindform
		 ,(rec bindforms*)))
     (()
      `(begin ,@body)))))

(define-macro* (lambda-pair bindforms . body)
  ;; for now just support a single argument pair. Should look into
  ;; generalizing lambda-values.
  (match-list*
   bindforms
   ((argpair1)
    ;;(with-gensym V  dependency sigh.
    (let ((V (gensym 'V)))
      (match-list*
       argpair1
       ((a r)
	`(lambda (,V)
	   (let-pair ((,a ,r) ,V)
		     ,@body))))))
   (_
    (source-error
     stx "lambda-pair: only a single argument pair supported for now"))))

(TEST
 > ((lambda-pair ((a b)) (vector a b)) (cons 9 10))
 #(9 10)
 )

(define-macro* (with-pair var* . body)
  (assert* symbol? var*
	   (lambda (var)
	     (let ((l (string-split (symbol->string var) #\.)))
	       (if (= (length l) 2)
		   `(let-pair (,(map string->symbol l) ,var*) ,@body)
		   (source-error
		    var*
		    "variable name does not contain exactly one dot"))))))


(define (fold-right/last fn fnlast tail lis)
  (let rec ((lis lis))
    (let-pair
     ((a lis*) lis)
     (if (null? lis*)
	 (fnlast a tail)
	 (fn a (rec lis*))))))

(TEST
 > (fold-right/last vector cons 'end '(1 2 3))
 #(1 #(2 (3 . end)))
 > (fold-right/last vector cons 'end '(1))
 (1 . end)
 > (with-exception-catcher type-exception?
			   (lambda () (fold-right/last vector cons 'end '())))
 #t
 )


;; definition see list-util-1

(TEST
 > (map/tail inc 'mytail '(1 2))
 (2 3 . mytail)
 )



(define (box-push! b val)
  (set-box! b (cons val (unbox b))))

;; (define (box-inc! b)
;;   (set-box! b (cons val (unbox b))))
;; inc! macro is not defined here.

(define-macro* (push! var-or-form val)
  (let ((var-or-form* (source-code var-or-form)))
    (cond ((symbol? var-or-form*)
	   (let ((var var-or-form))
	     `(set! ,var (cons ,val ,var))))
	  ((pair? var-or-form*)
	   (let ((a (source-code (car var-or-form*))))
	     (case a
	       ((unbox)
		(if (= (improper-length var-or-form*) 2)
		    `(box-push! ,(cadr var-or-form*) ,val)
		    (source-error var-or-form "unbox form of improper length")))
	       (else
		(source-error var-or-form "unknown kind of form")))))
	  (else
	   (source-error var-or-form "need variable name or a form")))))

(TEST
 > (define x '())
 > (push! x 1)
 > (push! x 2)
 > x
 (2 1)
 > (define x (box '()))
 > (push! (unbox x) 1)
 > (push! (unbox x) 2)
 > (unbox x)
 (2 1))

;; pop! see list-util-2


(define (list-preferred lis prefer?)
  (if (pair? lis)
      (let lp ((lis (cdr lis))
	       (min-val (car lis)))
	(cond ((pair? lis)
	       (lp (cdr lis)
		   (let ((val (car lis)))
		     (if (prefer? val
				  min-val)
			 val
			 min-val))))
	      ((null? lis)
	       min-val)
	      (else
	       (error "list-preferred: improper list:" lis))))
      (error "list-preferred: need non-empty list, got:" lis)))

(TEST
 > (list-preferred '(1 3 2 9 -8 3) <)
 -8
 > (list-preferred '(1 3 2 9 -8 3) >)
 9
 > (list-preferred '((1 a) (3 b) (2 c) (9 d) (-8 e) (3 f)) (lambda (a b) (> (car a) (car b))))
 (9 d)
 > (list-preferred '((1 a) (3 b) (2 c) (9 d) (-8 e) (10 f)) (lambda (a b) (> (car a) (car b))))
 (10 f)
 > (list-preferred '((1 a) (3 b) (2 c) (9 d) (-8 e) (9 f)) (lambda (a b) (> (car a) (car b))))
 (9 d)
 )

;; same as list-preferred but also return the list fragments after
;; (including) and before (excluding) the choosen element

(define (split-preferred lis prefer?)
  (if (pair? lis)
      (let lp ((l (cdr lis))
	       (rbefore (cons (car lis) '()))
	       (min-v (car lis))
	       (min-rest lis)
	       (min-rbefore '()))
	(cond ((pair? l)
	       (let* ((v (car l))
		      (r (cdr l))
		      (rbefore* (cons v rbefore)))
		 (if (prefer? v min-v)
		     (lp r rbefore*
			 v
			 l
			 rbefore)
		     (lp r rbefore*
			 min-v
			 min-rest
			 min-rbefore))))
	      ((null? l)
	       (values min-v min-rest min-rbefore))
	      (else
	       (error "improper list ending in:" l))))
      (error "need non-empty list, got:" lis)))
(TEST
 > (require (srfi-11)))
(TEST
 > (values->vector (split-preferred '(1) <))
 #(1 (1) ())
 > (values->vector (split-preferred '(1 2) <))
 #(1 (1 2) ())
 > (values->vector (split-preferred '(2 1) <))
 #(1 (1) (2))
 > (values->vector (split-preferred '(2 1 3) <))
 #(1 (1 3) (2))
 )

;; an append that reverses l1 when appending to l2
(define (rappend l1 l2)
  (let lp ((l1 l1)
	   (l l2))
    (cond ((pair? l1)
	   (lp (cdr l1)
	       (cons (car l1) l)))
	  ((null? l1)
	   l)
	  (else
	   (error "improper list l1 ending in:" l1)))))
(TEST
 > (rappend '() '(3))
 (3)
 > (rappend '(2) '(3))
 (2 3)
 > (rappend '(2 1) '(3))
 (1 2 3)
 )



;; for-each that returns the value(s) of the last call

(define (for-each*/2 proc l)
  (let lp ((v (void))
	   (l l))
    (if (null? l)
	v
	(let-pair ((v l*) l)
		  (lp (proc v) l*)))))

;; well for now just:
(define for-each* for-each*/2)

(TEST
 > (for-each* values '(a b c))
 c
 > (for-each* values '())
 #!void
 )


;; a split-at that doesn't die when requesting a split behind the end
;; of the list

(define (split-at* x k)
  (let recur ((lis x) (k k))
    (if (or (zero? k) (null? lis))
	(values '() lis)
	(receive (prefix suffix)
		 (recur (cdr lis) (- k 1))
		 (values (cons (car lis) prefix) suffix)))))

(TEST
 > (values->vector (split-at* '(a b c d) 3))
 #((a b c) (d))
 > (values->vector (split-at* '(a b c d) 4))
 #((a b c d) ())
 > (values->vector (split-at* '(a b c d) 5))
 #((a b c d) ())
 )


;; |every| is also in srfi-1; some is called |any| in srfi-1. Testing
;; short-cutting behaviour:

(TEST
 > (define *count 0)
 > (define (*even? v)
     (set! *count (inc *count))
     (even? v))
 > (every *even? '(1 3))
 #f
 > (every *even? '(1 2))
 #f
 > (every *even? '(2 1))
 #f
 > (every *even? '(2 4))
 #t
 > (every *even? '(2))
 #t
 > (every *even? '(1))
 #f
 > (every *even? '())
 #t
 > *count
 8
 )

(TEST
 > (set! *count 0)
 > (any *even? '(1 3))
 #f
 > (any *even? '(1 2))
 #t
 > (any *even? '(2 1))
 #t
 > (any *even? '(2 4))
 #t
 > (any *even? '(2))
 #t
 > (any *even? '(1))
 #f
 > (any *even? '())
 #f
 > *count
 8
 )


;; implementation of list-split see list-util-1

(TEST
 > (list-split '(a b c d e) 'c)
 ((a b) (d e))
 > (list-split '(a b c d e) 'x)
 ((a b c d e))
 > (list-split '(a b x c d x e) 'x)
 ((a b) (c d) (e))
 > (list-split '(a b x c d x e x) 'x)
 ((a b) (c d) (e) ())
 > (list-split '(x a b x c d x e x) 'x)
 (() (a b) (c d) (e) ())
 )



(define (rxtake-while pred lis) ;; dies if it reaches end of lis; output reversed
  (let lp ((res '())
	   (lis lis))
    (if (null? lis)
	(error "reached end of lis before finding pred")
	(let ((a (car lis)))
	  (if (pred a)
	      (lp (cons a res)
		  (cdr lis))
	      res)))))


(define (one? v)
  ;; require that a list is given
  (cond ((null? v)
	 #f)
	((pair? v)
	 (null? (cdr v)))
	(else
	 (error "not a list:" v))))

(define (xone x #!optional (fail (lambda_ #f)))
  (if (pair? x)
      (if (null? (cdr x))
	  (car x)
	  (fail 'found-too-many))
      (fail (if (null? x)
		'not-found
		'improper-list))))

(define (xxone x)
  (xone x (lambda (e)
	   (error "expected one item, but got:" e x))))

(define (trif-one x then/1 toomany/1 none/0)
  (if (pair? x)
      (if (null? (cdr x))
	  (then/1 (car x))
	  (toomany/1 x))
      (none/0)))

;; tests see list-util-test

(define (trif-one/ fn)
  (lambda (x then/1 toomany/1 none/0)
    (trif-one (fn x) then/1 toomany/1 none/0)))


;; (define (*if-one l *then *else)
;;   ;; hm, don't use trif-one since don't want to pass the values?
;;   )

;; wow can then 'wrap' that as syntax, too. Perhaps should provide
;; this functionality through some "hint"? (But then only efficient
;; when inlining *if-one.)
(define-macro* (if-one v then else)
  ;; `(*if-one ,v
  ;; 	    (lambda () ,then)
  ;; 	    (lambda () ,else))
  `(if (one? ,v)
       ,then
       ,else))

(TEST
 > (if-one (list) 'y 'n)
 n
 > (if-one (list 1) 'y 'n)
 y
 > (if-one (list 'a 'b) 'y 'n)
 n)


(define (make-list/tail n item tail)
  (let lp ((n n)
	   (res tail))
    (if (positive? n)
	(lp (dec n)
	    (cons item res))
	res)))

(TEST
 > (make-list/tail 3 "foo" '("bar"))
 ("foo" "foo" "foo" "bar")
 )


(define (map-apply fn listoflists)
  (map (lambda (l)
	 (apply fn l))
       listoflists))


(TEST
 > (map/iota cons '(a b))
 ((a . 0) (b . 1)))


(define (butlast l)
  (drop-right l 1))

(TEST
 > (butlast '(a b c))
 (a b)
 > (with-exception-catcher (lambda (e) #t) (lambda () (butlast '())))
 #t
 )

