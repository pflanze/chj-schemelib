

;; sigh url-encoding AGAIN. where did I have it before?? recently? no?

(define url-encoding:reserved-chars
  '(
    #\! #\*    #\'    #\(    #\)
	#\;	#\:	#\@	#\&	#\=	#\+	#\$	#\,	#\/	#\?	#\#	#\[	#\]))

(define-typed (url-encoding:reserved? #(char? c))
  (and (memq c url-encoding:reserved-chars) #t))

(define (url-encoding:unreserved? c)
  (let* ((is (lambda (c2)
	       (eq? c c2)))
	 (n (char->integer c))
	 (_in-range (cut <= <> n <>))
	 (in-range (on char->integer _in-range)))
    (or (in-range #\A #\Z)
	(in-range #\a #\z)
	(in-range #\0 #\9)
	(is #\-)
	(is #\_)
	(is #\.)
	(is #\~))))
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
  (compose* list->string fn string->list))

(define url-encode (stringliststring _url-encode))

(TEST
 > (url-encode "")
 ""
 > (url-encode " ")
 "%20"
 > (url-encode "foo/bar")
 "foo%2Fbar"
 > (url-encode "äbi")
 "%E4bi" ;; hmm ok?
 )

(define (path->url-encode str)
  (strings-join (map url-encode (string-split str #\/)) "/"))


(TEST
 > (path->url-encode "")
 ""
 > (path->url-encode "/")
 "/"
 > (path->url-encode "foo/ bar")
 "foo/%20bar"
 > (path->url-encode "foo/ bär")
 "foo/%20b%E4r" ;; XXX hmmm wrong right? sigh.
 )

