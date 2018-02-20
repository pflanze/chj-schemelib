;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test
	 (test-lib-1 %try-error)
	 C
	 srfi-1
	 (cj-env-1 dec inc identity)
	 (string-util-1 string-split)
	 (improper-list improper-length)
	 ;;(cj-env-2 xcase) ah, cycle
	 (lazy FV))

(export (macro let-pair
	  let*-pair
	  lambda-pair
	  with-pair)
	fold-right/last
	box-push!
	(macro push!)
	list-preferred
	split-preferred
	rappend
	for-each* ;; for-each*/2, but what sense did this name have?
	split-at*
	rxtake-while
	one-item?
	xone ;; also see |the| in easy-1
	xone/fail
	maybe-xone
	trif-one
	trif-one/
	make-list/tail
	map-apply
	butlast
	map/sides
	map/sides?
	repeatedly
	natural0-fold
	fold/fn0
	#!optional
	_map/sides
	_map/sides?
	map/maybe-sides)


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

(define-macro* (let*-pair bindforms . body)
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


;; Also see stream-min&max and derived functions incl list-min etc.
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
 > (list-split '(a aa b c d) 'b #t)
 ((a aa) b (c d)))



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


(define (one-item? v)
  ;; require that a list is given
  (cond ((null? v)
	 #f)
	((pair? v)
	 (or (null? (cdr v))
	     (null? (force (cdr v)))))
	((promise? v)
	 (one-item? (force v)))
	(else
	 (error "not a list:" v))))

;; can't use cj-inline yet (circular dependency)
(define (xone/fail x fail)
  (FV (x)
      (if (pair? x)
	  (if (null? (force (cdr x)))
	      (car x)
	      (fail 'found-too-many))
	  (fail (if (null? x)
		    'not-found
		    'improper-list)))))

(define (xone x)
  (xone/fail x
	     (lambda (e)
	       (error "expected one item, but:" e
		      (force x)))))

(TEST
 > (%try-error (xone (delay '())))
 #(error "expected one item, but:" not-found ())
 > (%try-error (xone (delay '(a b))))
 #(error "expected one item, but:" found-too-many (a b))
 > (def (t v)
	(list (%try-error (one-item? v))
	      (xone/fail v identity)))
 > (t (delay (list 1 2)))
 (#f found-too-many)
 > (t (delay (list 2)))
 (#t 2)
 > (t (delay '()))
 (#f not-found)
 > (t (delay (cons 1 (delay (cons 2 (delay '()))))))
 (#f found-too-many)
 > (t (delay (cons 1 (delay (delay '())))))
 (#t 1)
 > (t (delay (cons 1 (delay '()))))
 (#t 1)
 > (t (delay (cons 1 (delay 2))))
 ;; improper-list ah, doesn't check for that case, oh well should be fine:
 (#f found-too-many)
 > (t (cons 1 (delay '())))
 (#t 1)
 > (t (delay 1))
 (#(error "not a list:" 1) improper-list)
 > (t (cons 1  '()))
 (#t 1))


(define (maybe-xone v)
  (xone/fail v (lambda (e)
		 (case e
		   ((not-found) #f)
		   (else
		    (error "expected one item or none, but:" e
			   (force v)))))))

(TEST
 > (maybe-xone '())
 #f
 > (maybe-xone '(a))
 a
 > (%try-error (maybe-xone (iota 2)))
 #(error "expected one item or none, but:" found-too-many (0 1)))


;; XX change to handle streams, too?
(define (trif-one x then/1 toomany/1 none/0)
  (if (pair? x)
      (if (null? (cdr x))
	  (then/1 (car x))
	  (toomany/1 x))
      (none/0)))

(TEST
 > (%try-error
    (trif-one '(a . b) identity (C error "too many:" _) (C error "none")))
 #(error "too many:" (a . b))
 > (%try-error
    (trif-one '(a) identity (C error "too many:" _) (C error "none")))
 a
 > (%try-error
    (trif-one '() identity (C error "too many:" _) (C error "none")))
 #(error "none")
 )

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
  `(if (one-item? ,v)
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


;; deprecated, use (map (applying fn) ..)
(define (map-apply fn listoflists)
  (map (lambda (l)
	 (apply fn l))
       listoflists))


(TEST
 > (map/iota cons '(a b))
 ((a . 0) (b . 1)))

(TEST
 > (filter/iota (lambda (x i) (and (positive? x) (even? i))) '(4 3 2 1 0 -1 -2 -3 -4))
 (4 2)
 > (filter/iota (lambda (x i) (and (negative? x) (even? i))) '(4 3 2 1 0 -1 -2 -3 -4))
 (-2 -4))

(TEST
 > (define v (make-vector 5 #f))
 > (for-each/iota (lambda (val i)
		    (vector-set! v i val))
		  '(a b c d))
 > v
 #(a b c d #f)
 ;; and another test I wrote, now moved here (superfluous?):
 > (define r '())
 > (for-each/iota (lambda (v i) (push! r (list v i))) '(a b c))
 #!void
 > r
 ((c 2) (b 1) (a 0)) )


(define (butlast l)
  (drop-right l 1))

(TEST
 > (butlast '(a b c))
 (a b)
 > (with-exception-catcher (lambda (e) #t) (lambda () (butlast '())))
 #t
 )

(TEST
 > (map/last? vector '())
 ()
 > (map/last? vector '(a b c))
 (#(#f a) #(#f b) #(#t c)))

(TEST
 > (reverse-map inc '(1 2 3))
 (4 3 2)
 > (reverse-map/tail inc '(1 2 3) 'b)
 (4 3 2 . b))



(define (_map/sides fn l left tail)
  (if (null? l)
      tail
      (let-pair ((a r) l)
		(cons (fn a left r)
		      (_map/sides fn r (cons a left) tail)))))

(define (map/sides fn l)
  (_map/sides fn l '() '()))

(TEST
 > (map/sides list '(a b c))
 ((a () (b c))
  (b (a) (c))
  (c (b a) ()))
 > (map/sides list '())
 ())


(define (_map/sides? fn l left? tail)
  (if (null? l)
      tail
      (let-pair ((a r) l)
		(cons (fn a left? (null? r))
		      (_map/sides? fn r #f tail)))))

(define (map/sides? fn l)
  (_map/sides? fn l #t '()))

(TEST
 > (map/sides? list '(a b c))
 ((a #t #f)
  (b #f #f)
  (c #f #t))
 > (map/sides? list '())
 ())

(define (_map/maybe-sides fn l maybe-left tail)
  (if (null? l)
      tail
      (let-pair ((a r) l)
		(cons (fn a maybe-left (if (null? r) #f (first r)))
		      (_map/maybe-sides fn r a tail)))))

(define (map/maybe-sides fn l)
  (_map/maybe-sides fn l #f '()))

(TEST
 > (map/maybe-sides list '(a b c))
 ((a #f b)
  (b a c)
  (c b #f))
 > (map/maybe-sides list '())
 ())


(TEST
 > (flatten1 '(a (b c) (d (e)) f))
 (a b c d (e) f)
 > (flatten1 (flatten1 '(a (b c) (d (e)) f)))
 (a b c d e f))



(define repeatedly-noval (gensym 'noval))

(define (repeatedly n fn #!optional (val repeatedly-noval))
  (define (rep val)
    (let lp ((i n)
	     (val val))
      (if (positive? i)
	  (lp (dec i)
	      (fn val))
	  val)))
  (if (eq? val repeatedly-noval)
      rep
      (rep val)))

(TEST
 > (repeatedly 10 inc 5)
 15
 > ((repeatedly 4 square) 2)
 65536)


(define (natural0-fold fn start n)
  (if (positive? n)
      (natural0-fold fn (fn n start) (dec n))
      start))

(TEST
 > (natural0-fold cons '(end) 5)
 (1 2 3 4 5 end))


(define (fold/fn0 fn fn0 vs)
  (let-pair ((v0 vs*) vs)
	    (fold fn
		  (fn0 v0)
		  vs*)))

