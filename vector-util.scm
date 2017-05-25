;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
	 (cj-env-2 for..<)
	 test
	 ;; vector-util-1 ;; well, cj-source or mod/mod.scm since those include it?
	 srfi-1
	 (list-util-1 map/iota))


(export vectors-map
	vector-map
	vector-map*
	;; note: dot-oo versions of these are now also in oo-vector-lib
	vector-fold-right
	vector-fold
	vector-for-each
	vector-generate
	if-avector-ref
	maybe-avector-ref
	;; note: not OO:
	vector.set
	vector.insert
	vector-every
	vector-of
	vector-of/length
	;; note: not oo, just curried:
	vector.value.pos)



(define (vectors-map fn vecs accept-uneven-lengths?)
  (let* ((lens (map vector-length vecs))
	 (len (apply min lens))
	 (cont (lambda ()
		 (let* ((res (make-vector len)))
		   (let lp ((i 0))
		     (if (= i len)
			 res
			 (begin
			   (vector-set! res
					i
					(apply fn
					       (map (lambda (vec)
						      (vector-ref vec i))
						    vecs)))
			   (lp (inc i)))))))))
    (if accept-uneven-lengths?
	(cont)
	(let ((lenmax (apply max lens)))
	  (if (= len lenmax)
	      (cont)
	      (error "uneven lengths of input vectors (min max):"
		     len lenmax))))))

(define (vector-map fn . vecs)
  (vectors-map fn vecs #f))

(define (vector-map* fn . vecs)
  (vectors-map fn vecs #t))

(TEST
 > (vector-map + '#(1 2 3) '#(3 4 5))
 #(4 6 8)
 > (with-exception-catcher error-exception-message (lambda () (vector-map + '#(1 2 3) '#(3 4))))
 "uneven lengths of input vectors (min max):"
 > (vectors-map + '(#(1 2 3) #(3 4)) #t)
 #(4 6)
 > (vector-map* + '#(1 2 3) '#(3 4))
 #(4 6)
 )


;; NOTE: dot-oo versions of these are now also in oo-vector-lib
(define (vector-fold-right fn tail vec)
  (let ((len (vector-length vec)))
    (let rec ((i 0))
      (if (= i len)
	  tail
	  (fn (vector-ref vec i)
	      (rec (inc i)))))))

(define (vector-fold fn tail vec)
  (let ((len (vector-length vec)))
    (let lp ((res tail)
	     (i 0))
      (if (= i len)
	  res
	  (lp (fn (vector-ref vec i)
		  res)
	      (inc i))))))

(TEST
 > (fold vector 'null '(1 2 3))
 #(3 #(2 #(1 null)))
 > (vector-fold vector 'null '#(1 2 3))
 #(3 #(2 #(1 null)))
 > (fold-right vector 'null '(1 2 3))
 #(1 #(2 #(3 null)))
 > (vector-fold-right vector 'null '#(1 2 3))
 #(1 #(2 #(3 null)))
 )

(define (vector-for-each proc vec)
  (vector-fold (lambda (v _)
		 (proc v))
	       #f
	       vec))

;; /oo-vector-lib


;; (define (vector-unfold end? fmap fnext start)
;;   )
;; has the problem of not knowing how many elements it will contain in advance.
;; hence rather: vector-generate

(define (vector-generate size fn/i)
  (let ((res (make-vector size)))
    (let lp ((i 0))
      (if (< i size)
	  (begin
	    (vector-set! res i (fn/i i))
	    (lp (inc i)))))
    res))

(TEST
 > (vector-generate 10 square)
 #(0 1 4 9 16 25 36 49 64 81)
 )


;; (define (vector/list.for-each proc/i+V+L starti endi v l)
;;   ;; (for..< (i starti endi)
;;   ;; 	  )
;;   (let lp ((i starti)
;; 	   (l l))
;;     (if (< i endi)
;; 	(begin
;; 	  (proc/V+L i
;; 		    (vector-ref v i)
;; 		    (car l))
;; 	  (lp (inc i)
;; 	      (cdr l))))))


;; association vectors: vectors of some kind of tuple; access is the
;; analogon of a car of an alist (could be car, of course; but structs
;; more 'common'?)

(define (if-avector-ref vec starti maybe-endi access equal? val
			then/i+tuple else/0)
  (let ((endi (or maybe-endi (vector-length vec))))
    ;; can't use for..< here because of not having a 'return'.
    (let lp ((i starti))
      (if (< i endi)
	  (let ((tupl (vector-ref vec i)))
	    (if (equal? (access tupl) val)
		(then/i+tuple i tupl)
		(lp (inc i))))
	  (else/0)))))

(define (maybe-avector-ref vec starti maybe-endi access equal? val)
  (if-avector-ref vec starti maybe-endi access equal? val
		  (lambda (i tuple)
		    tuple) false/0))

(TEST
 > (maybe-avector-ref '#(foo (a 1) (c 2) (b 10)) 1 #f car eq? 'c)
 (c 2)
 > (maybe-avector-ref '#(foo (a 1) (c 2) (b 10)) 1 #f car eq? 'd)
 #f)


;; functional vector modification

(define (vector.set v i val)
  ;; (check whether val changes?)
  (if (eq? (vector-ref v i) val)
      v
      (let ((v* (vector-copy v)))
	(##vector-set! v* i val)
	v*)))

(define (vector.insert v i val)
  (let* ((len (vector-length v))
	 (len* (inc len))
	 (v* (make-vector len*)))
    (for..< (j 0 i)
	    (vector-set! v* j (vector-ref v j)))
    (vector-set! v* i val)
    (for..< (j (inc i) len*)
	    (vector-set! v* j (vector-ref v (dec j))))
    v*))

(TEST
 > (vector.insert (vector) 0 'a)
 #(a)
 > (vector.insert (vector 1 2) 0 'a)
 #(a 1 2)
 > (vector.insert (vector 1 2) 1 'a)
 #(1 a 2)
 > (vector.insert (vector 1 2) 2 'a)
 #(1 2 a))


(define (vector-every t? v)
  (let ((len (vector-length v)))
    (let lp ((i 0))
      (if (< i len)
	  (and (t? (vector-ref v i))
	       (lp (inc i)))
	  #t))))


(define (vector-of t?)
  (lambda (v)
    (and (vector? v)
	 (vector-every t? v))))

;; vgl. u8vector-of-length, but here we want a type check as well
(define (vector-of/length t? len)
  (lambda (v)
    (and (vector? v)
	 (= (vector-length v) len)
	 (vector-every t? v))))

(TEST
 > (map (vector-of integer?)
	'(#() #(#f) #(#t) 1
	   #(a)
	   #(1 2 3)
	   #(1 2 a)
	   #(a 2 3)
	   #(1 a 3)))
 (#t #f #f #f
     #f #t #f #f #f))



(define vector-util:no-alternative (gensym "no-alternative"))

(define (vector.value.pos vs)
  (let ((t (list->table (map/iota (lambda (s i)
				    (cons s i))
				  (vector->list vs)))))
    (lambda (k #!optional (alternative vector-util:no-alternative))
      (let ((v (table-ref t k alternative)))
	(if (eq? v vector-util:no-alternative)
	    (error "unknown key:" k)
	    v)))))

