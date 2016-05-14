;;; Copyright 2013 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require test
	 cj-env-1)

(export u8-rlist->string
	u8-rlist->u8vector
	read-u8-until
	read-u8-line
	u8-whitespace?
	read-u8-word
	read-u8-u8line
	read-u8-integer
	;; huh, how comes mk-read-all is not used anywhere?

	#!optional
	premature-eof
	mk-read-all)


(define (u8-rlist->_ make-vec vec-set!)
  (lambda (lis #!optional (len (length lis)))
    (let ((res (make-vec len)))
      (let lp ((lis lis)
	       (i (dec len)))
	(if (null? lis)
	    res
	    (begin
	      (vec-set! res i (car lis))
	      (lp (cdr lis)
		  (dec i))))))))

(define u8-rlist->string
  (u8-rlist->_ ##make-string
	       (lambda (vec i val)
		 (string-set! vec i (integer->char val)))))

(define u8-rlist->u8vector
  (u8-rlist->_ ##make-u8vector
	       (lambda (vec i val)
		 (u8vector-set! vec i val))))

(define (read-u8-until p pred
		       #!key
		       on-eof
		       (tail '()))
  ;; (values rlist-of-characters-before-matching-pred len char-matching-pred)
  (let lp ((res tail)
	   (len 0))
    (let ((v (read-u8 p)))
      (if (and on-eof
	       (eof-object? v))
	  (on-eof)
	  (if (pred v)
	      (values res len v)
	      (lp (cons v res)
		  (inc len)))))))

(define premature-eof (thunk (error "premature eof")))

(define (read-u8-line p)
  (letv ((rlis len c) (read-u8-until p (cut = <> 10)
				     on-eof: premature-eof))
	(u8-rlist->string rlis len)))


(define (u8-whitespace? v)
  (or (fx= v 32)
      (fx= v 10)
      (fx= v 9)))

(TEST
 > (u8-whitespace? (.integer #\tab))
 #t
 > (u8-whitespace? (.integer #\a))
 #f
 )


(define (read-u8-word p)
  (letv ((rlis len c) (read-u8-until p u8-whitespace?
				     on-eof: premature-eof))
	(u8-rlist->string rlis len)))

(define (read-u8-u8line p)
  (letv ((rlis len c) (read-u8-until p (cut = <> 10)
				     on-eof: premature-eof))
	(u8-rlist->u8vector rlis len)))

(define (read-u8-integer p)
  ;; dropping followup char on the ground!
  (letv ((rpre _len c) (read-u8-until p (either eof-object?
						(lambda (v)
						  (<= 48 v 57)))))
	(if (eof-object? c)
	    c
	    (letv ((r len _c) (read-u8-until p (either eof-object?
						       (lambda (v)
							 (not (<= 48 v 57))))
					     tail: (list c)))
		  (string->number (u8-rlist->string r (inc len)))))))


(TEST
 > (call-with-input-u8vector (.u8vector "foo bar\n") read-u8-line)
 "foo bar"
 > (call-with-input-u8vector (.u8vector "foo bar\n") read-u8-word)
 "foo"
 )

(define (mk-read-all read)
  (lambda (p)
    (let lp ((res '()))
      (let ((v (read p)))
	(if (eof-object? v)
	    (reverse res)
	    (lp (cons v res)))))))


(TEST
 > (call-with-input-u8vector '#u8(65 66 67 10 44) read-u8-line)
 "ABC"
 > (call-with-input-u8vector '#u8(65 66 67 10 44) read-u8-integer)
 #!eof
 > (call-with-input-u8vector '#u8(49 50 10 44) read-u8-integer)
 12
 )

