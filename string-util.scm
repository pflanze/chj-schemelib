;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.test))


(define (r-list-split l separator)
  (let lp ((l l)
	   (cum '())
	   (res '()))
    (if (null? l)
	(cons cum res)
	(let-pair
	 ((a r) l)
	 (if (equal? a separator)
	     (lp r
		 '()
		 (cons cum res))
	     (lp r
		 (cons a cum)
		 res))))))

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

(define (list-split l separator)
  (reverse (map reverse (r-list-split l separator))))

(define (string-split str char)
  (map list->string (list-split (string->list str) char)))

(define (lists-join ls seplis #!optional (tail '()))
  (let rec ((ls ls))
    (if (null? ls)
	tail
	(let-pair
	 ((l ls*) ls)
	 (if (null? ls*)
	     (append l (rec ls*))
	     (append l seplis (rec ls*)))))))

(TEST
 > (lists-join '((a b) (d) (e f)) '(1 2))
 (a b 1 2 d 1 2 e f)
 )

(define (strings-join strs str)
  (list->string (lists-join (map string->list strs) (string->list str))))
