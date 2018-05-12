;; sigh url-encoding AGAIN. where did I have it before?? recently? no?

(require fixnum
	 cj-typed
	 test
	 cj-functional
	 (srfi-11 letv)
	 srfi-1
	 (string-util strings-join string-split)
	 u8vector0
	 (hex hexdigit char.parse-hexdigit)
	 (oo-lib-string u8vector.string))

(export url-encode url-encode-u8vector
	url-decode
	path-string.url-encode
	url-string-and-fragment)

(include "cj-standarddeclares.scm")


(define-macro (CHAR->INTEGER c)
  (char->integer c))


(define-typed (url-encoding:unreserved? #(char? c))
  (declare (not safe) (fixnum))
  (case c
    ((#\- #\_ #\. #\~) #t)
    (else
     (let ((cn (char->integer c)))
       (or (<= (CHAR->INTEGER #\a)
	       cn
	       (CHAR->INTEGER #\z))
	   (<= (CHAR->INTEGER #\A)
	       cn
	       (CHAR->INTEGER #\Z))
	   (<= (CHAR->INTEGER #\0)
	       cn
	       (CHAR->INTEGER #\9)))))))

(TEST
 > (url-encoding:unreserved? #\c)
 #t
 > (url-encoding:unreserved? #\/)
 #f
 > (url-encoding:unreserved? #\-)
 #t
 )

(define (fold:number->hex len n tail)
  ;; ensure even padding right?
  (if (zero? n)
      (cond ((zero? len)
	     (cons #\0 (cons #\0 tail)))
	    ((even? len)
	     tail)
	    (else
	     (cons #\0 tail)))
      (letv ((n* digit) (quotient+modulo n 16))
	    (fold:number->hex (inc len)
			      n*
			      (cons (hexdigit digit)
				    tail)))))

(TEST
 > (fold:number->hex 0 137 '())
 (#\8 #\9)
 > (fold:number->hex 0 255 '())
 (#\F #\F)
 > (fold:number->hex 0 256 '())
 (#\0 #\1 #\0 #\0)
 > (fold:number->hex 0 0 '())
 (#\0 #\0)
 )

(define (_url-encode lis)
  (fold-right (lambda (c res)
		(if (url-encoding:unreserved? c)
		    (cons c res)
		    (cons #\%
			  (fold:number->hex 0 (char->integer c) res))))
	      '()
	      lis))

(define (stringliststring fn)
  (compose list->string
	    fn
	    (lambda (l)
	      (map integer->char l))
	    u8vector->list
	    string.utf8-u8vector))

(define url-encode-slow (stringliststring _url-encode))

(define cj-url-encode:url-encode-retry 0)

(define-typed (url-encode-u8vector [string? str])
  (declare (fixnum))

  ;; Optimistic (?, attempting) algorithm: allocate a fixed buffer, if
  ;; the result fits, shrink it, otherwise (overflow case) retry with
  ;; larger buffer.

  (let* ((max-utf-bytes 4)
	 (buf (##make-u8vector max-utf-bytes))

	 (len (string-length str)))

    (let retry ((len* (+ 8 (* len 3))))
  
      (def (overflow)
	   (inc! cj-url-encode:url-encode-retry)
	   (retry (* len* 2)))

      (let* ((out (##make-u8vector len*)))
	
	(let lp ((i 0)
		 (i* 0))
	  (if (< i len)
	      (let* ((c (string-ref str i))
		     (utf8len (@u8vector-utf8-put! buf 0
						   (char->integer c))))
		(assert (fx<= utf8len max-utf-bytes))
		;; ^ mem already corrupted though
		(let lp-buf ((j 0)
			     (i* i*))
		  (if (< j utf8len)
		      (let ((b (u8vector-ref buf j)))
			(if (url-encoding:unreserved? (integer->char b))
			    (if (< i* len*)
				(begin
				  (u8vector-set! out i* b)
				  (lp-buf (inc j)
					  (inc i*)))
				(overflow))
			    (if (<= (+ i* 3) len*) ;; XX <= ?
				(begin
				  (u8vector-set! out i* (CHAR->INTEGER #\%))
				  (u8vector-set!
				   out (+ i* 1)
				   (hexdigit-integer (arithmetic-shift b -4)))
				  (u8vector-set!
				   out (+ i* 2)
				   (hexdigit-integer (bitwise-and b 15)))
				  (lp-buf (inc j)
					  (+ i* 3)))
				(overflow))))
		      (lp (inc i)
			  i*))))
	      (begin
		(u8vector-shrink! out i*)
		out)))))))

(define (url-encode str)
  ;; somewhat wasteful..
  (u8vector.string (url-encode-u8vector str)))

(TEST
 > (url-encode "")
 ""
 > (url-encode " ")
 "%20"
 > (url-encode "foo/bar")
 "foo%2Fbar"
 > (url-encode "äbi")
 "%C3%A4bi"
 > (url-encode (string (integer->char 3485)))
 "%E0%B6%9D"
 )

(define (path-string.url-encode str)
  (strings-join (map url-encode (string-split str #\/)) "/"))


(TEST
 > (path-string.url-encode "")
 ""
 > (path-string.url-encode "/")
 "/"
 > (path-string.url-encode "foo/ bar")
 "foo/%20bar"
 > (path-string.url-encode "foo/ bär")
 "foo/%20b%C3%A4r"
 )


(define (url-decode str) ;; gives exceptions on invalid input, so don't compile unsafe..
  (u8vector.utf8-parse
   (list->u8vector
    (map char->integer
	 (let rec ((l (string->list str)))
	   (if (null? l)
	       l
	       (let-pair ((a r) l)
			 (if (char=? a #\%)
			     (let-pair ((b r) r)
				       (let-pair ((c r) r)
						 (cons (integer->char
							(+ (* 16 (char.parse-hexdigit b))
							   (char.parse-hexdigit c)))
						       (rec r))))
			     (cons a (rec r))))))))))

(TEST
 > (url-decode (url-encode " foo bar"))
 " foo bar"
 > (url-decode (url-encode " foo bar&-() !\" 87"))
 " foo bar&-() !\" 87"
 > (url-decode (url-encode (string #\% (integer->char 250) #\%)))
 "%\372%"
 > (string->number "372" 8)
 250)



(define-typed (url-string-and-fragment #(string? url-string) #(string? fragment))
  ;; XXX do we need to escape fragment somehow ?
  (string-append url-string "#" fragment))

(TEST
 > (url-string-and-fragment (url-encode "foo#bar") "baz#1")
 "foo%23bar#baz#1")

