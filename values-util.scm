;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-phasing
         cj-env
         (srfi-1 iota)
	 values)

(export zip-values
	;; zip-values/0 and /1 are not useful so don't generate them ok?
	zip-values/2
	zip-values/3
	zip-values/4
	zip-values/5
	lazy-values-split/2)



;; XX but should this be in list-util or so instead? sigh ~forever.

(compile-time
 (define (zip-values-code-for i)
   (let ((name (symbol-append 'zip-values/ (.string i)))
         (vs (map (compose gensym .string) (iota i 1))))
     `(define (,name ,@vs)
        (if (and ,@(map (lambda (v)
                          `(null? ,v))
                        vs))
            '()
            (cons (values ,@(map (lambda (v)
                                   `(car ,v))
                                 vs))
                  (,name ,@(map (lambda (v)
                                  `(cdr ,v))
                                vs))))))))

(insert-result-of
 `(begin
    ,@(map zip-values-code-for (.. 2 5))))


(define (zip-values a b . rest)
  (if (null? rest)
      (zip-values/2 a b)
      (if (null? (cdr rest))
          (zip-values/3 a b (car rest))
          (if (null? (cddr rest))
              (zip-values/4 a b (car rest) (cadr rest))
              (if (null? (cdddr rest))
                  (zip-values/5 a b (car rest) (cadr rest) (caddr rest))
                  (error "zip-values: too many arguments"))))))


(TEST
 > (define l (zip-values '(a b) '(1 2)))
 > (map fst l)
 (a b)
 > (map snd l)
 (1 2)
 > (define l (zip-values '(a b) '(1 2) '(#t #f) '("a" "b") '(-1 -2)))
 > (map fst l)
 (a b)
 > (map snd l)
 (1 2)
 > (map 3rd l)
 (#t #f)
 > (map 4th l)
 ("a" "b")
 > (map 5th l)
 (-1 -2)
 > (%try (zip-values '(a b)))
 (exception
  text:
  "Wrong number of arguments passed to procedure\n(zip-values '(a b))\n")
 > (%try-error (zip-values '(a b) '(1 2) '(#t #f) '("a" "b") '(-1 -2) '(9 8)))
 #(error "zip-values: too many arguments")
 > (%try (zip-values '(a b) '(1 2 3)))
 (exception text: "(Argument 1) PAIR expected\n(car '())\n")
 ;; XX better message?
 )



(define (lazy-values-split/2 v)
     (values (delay (fst (force v)))
	     (delay (snd (force v)))))

(TEST
 ;; Making a pure recursive multi-value return function lazy. ("udo"
 ;; like "up down", well.)
 > (define (udo f l)
	(lazy-values-split/2
	 (delay
	   (FV (l)
	       (if (null? l)
		   (values '() #t)
		   (let-pair ((a l*) l)
			     (cond ((f a)
				    => (lambda (v)
					 (letv ((vs OK?)
						(lazy-values-split/2 (udo f l*)))
					       (values (cons v vs)
						       OK?))))
				   (else
				    (values '() #f)))))))))
 > (define c 0)
 > (define (f x)
     (inc! c)
     (if (negative? x)
         #f
         (- x)))
 > (define t (compose* show F (C udo f _)))
 > (t '(1 2))
 (values (list -1 -2) #t)
 > (t '(1 -1 2))
 (values (list -1) #f)
 > c
 4
 > (promise? (fst (udo f '(1 2))))
 #t
 > c
 4
 > (force (snd (udo f '(1 2 3))))
 #t
 > c
 7
 > (stream-length (fst (force (udo f '(1 2 -3 4)))))
 2
 > c
 10
 > (force (snd (udo f '(1 2 -3 4))))
 #f
 > c
 13)


;; how the interesting questions are, how is this optimizable and then
;; how does it compare to iteration or what. And secondly, how do the
;; semantics compare to the latter.

