(require cj-typed
	 test
	 cj-functional
	 (srfi-11 letv)
	 srfi-1
	 (string-util strings-join string-split)
	 u8vector0)

(export url-encode
	url-decode
	path-string.url-encode
	url-string-and-fragment)


;; sigh url-encoding AGAIN. where did I have it before?? recently? no?

(define url-encoding:reserved-chars
  '(
    #\! #\*    #\'    #\(    #\)
	#\;	#\:	#\@	#\&	#\=	#\+	#\$	#\,	#\/	#\?	#\#	#\[	#\]))

(define-typed (url-encoding:reserved? #(char? c))
  (and (memq c url-encoding:reserved-chars) #t))

(define-typed (url-encoding:unreserved? #(char? c))
  (declare (not safe) (fixnum))
  (case c
    ((#\- #\_ #\. #\~) #t)
    (else
     (let ((cn (char->integer c)))
       (or (<= (insert-result-of (char->integer #\a))
	       cn
	       (insert-result-of (char->integer #\z)))
	   (<= (insert-result-of (char->integer #\A))
	       cn
	       (insert-result-of (char->integer #\Z)))
	   (<= (insert-result-of (char->integer #\0))
	       cn
	       (insert-result-of (char->integer #\9))))))))

(TEST
 > (url-encoding:unreserved? #\c)
 #t
 > (url-encoding:unreserved? #\/)
 #f
 > (url-encoding:unreserved? #\-)
 #t
 )

(define (hexdigit digit)
  (if (< digit 10)
      (integer->char (+ digit (char->integer #\0)))
      (integer->char (+ (- digit 10) (char->integer #\A)))))

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
  (compose* list->string
	    fn
	    (lambda (l)
	      (map integer->char l))
	    u8vector->list
	    string.utf8-u8vector))

(define url-encode (stringliststring _url-encode))

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

