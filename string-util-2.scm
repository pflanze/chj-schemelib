(define (suffix-list l)
  (rxtake-while (lambda (x)
                 (not (char=? x #\.)))
               (reverse l)))

(define suffix (compose* list->string suffix-list string->list))

(TEST
 > (suffix "foo.scm")
 "scm"
 )


;; slow way. just  .
;; drop-while but from the end.
(define (list-trim-right lis pred)
  ((compose* reverse
	     (cut drop-while pred <>)
	     reverse) lis))


(define string-trim-right
  (compose* list->string
	    (cut list-trim-right <> char-whitespace?)
	    string->list))

(TEST
 > (string-trim-right "")
 ""
 > (string-trim-right " ")
 ""
 > (string-trim-right "foo\n")
 "foo"
 > (string-trim-right "foo\n ")
 "foo"
 )


(define (string-ref* str i)
  (if (negative? i)
      (let ((len (string-length str)))
	(string-ref str (+ len i)))
      (string-ref str i)))
(TEST
 > (string-ref* "abc" 0)
 #\a
 > (string-ref* "abc" 1)
 #\b
 > (string-ref* "abc" -1)
 #\c
 > (string-ref* "abc" -2)
 #\b
 > (with-exception-handler range-exception? (thunk (string-ref* "abc" -4)))
 #t
 )

(define (string-empty? str)
  (zero? (string-length str)))

;; A string chom that works like Perl's

(define (chomp str)
  (if (string-empty? str)
      str
      (if (char=? (string-ref* str -1) #\newline) ;; Perl even ignores \r heh
	  (substring str 0 (dec (string-length str)))
	  str)))

(TEST
 > (chomp "")
 ""
 > (chomp " ")
 " "
 > (chomp "a\n")
 "a"
 > (chomp "a\n\n")
 "a\n"
 > (chomp "a\r")
 "a\r"
 )

;; ok?
(define trim string-trim-right)

(define trim-maybe (_-maybe trim))

(define char-newline?
  (cut char=? <> #\newline))

(define trimlines
  (compose* (cut strings-join <> "\n")
	    (cut map trim <>)
	    (cut string-split <> char-newline?)))

(define trimlines-maybe (_-maybe trimlines))

(TEST
 > (trimlines " Hello \nWorld. \n")
 " Hello\nWorld.\n"
 > (trimlines " Hello \nWorld.\n")
 " Hello\nWorld.\n"
 > (trimlines " Hello \nWorld.\n ")
 " Hello\nWorld.\n"
 )


;; hm stupid, basically same thing. With (back to) full names, which
;; is good, and trimming the end too, which may be good too.
(define (string-trimlines-right str)
  (strings-join (map string-trim-right
		     (string-split (string-trim-right str)
				   #\newline))
		"\n"))

(TEST
 > (string-trimlines-right " ")
 ""
 > (string-trimlines-right "")
 ""
 > (string-trimlines-right "foo \nbar ")
 "foo\nbar"
 > (string-trimlines-right "foo \nbar \n \n\n")
 "foo\nbar"
 > (string-trimlines-right "foo \nbar \n \n\nbaz")
 "foo\nbar\n\n\nbaz"
 > (string-trimlines-right " foo \nbar \n \n\nbaz")
 " foo\nbar\n\n\nbaz"
 )



(define (nonempty? v)
  (and v
       (not (string-empty? (trim v)))))

;; bad name? but:
;; - maybe handling
;; - string empty handling
;; - but *also* trimming.
;; Too much for web stuff? dunno

(TEST
 > (nonempty? #f)
 #f
 > (nonempty? "")
 #f
 > (nonempty? " ")
 #f
 > (nonempty? "\n")
 #f
 > (nonempty? "f\n")
 #t
 )


(define (string-multiply str n)
  (let* ((len (string-length str))
	 (out (##make-string (* len n))))
    (for..< (i 0 n)
	    (for..< (j 0 len)
		    (string-set! out
				 (+ (* i len) j)
				 (string-ref str j))))
    out))

(TEST
 > (string-multiply "ab" 3)
 "ababab"
 )

(define-typed (number->padded-string #(natural? width)
				     #(natural0? x))
  (let* ((s (number->string x))
	 (len (string-length s)))
    (string-append (string-multiply "0" (max 0 (- width len)))
		   s)))

(TEST
 > (number->padded-string 3 7)
 "007"
 > (number->padded-string 3 713)
 "713"
 > (number->padded-string 3 7132)
 "7132"
 > (number->padded-string 3 0)
 "000"
 )

(define-typed (inexact.number-format x #(natural0? left) #(natural0? right))
  (let* ((str (number->string x))
	 (len (string-length str)))
    (letv ((before after) (string-split-1 str #\.))
	  ;; after contains the dot, too
	  (let* ((lenbefore (string-length before))
		 (lenafter (string-length after))
		 (after* (substring after 0 (inc (min (dec lenafter) right))))
		 ;; ^  OH should do rounding XXX
		 (lenafter* (string-length after*)))
	    (string-append (string-multiply " " (max 0 (- left lenbefore)))
			   before
			   after*
			   (string-multiply " " (- right (dec lenafter*))))))))

(TEST
 > (inexact.number-format 3.456 3 3)
 "  3.456"
 > (inexact.number-format 3.456 3 4)
 "  3.456 "
 > (inexact.number-format 3.456 3 2)
 "  3.45"
 > (inexact.number-format 3.456 2 2)
 " 3.45"
 > (inexact.number-format 3.456 1 2)
 "3.45"
 > (inexact.number-format 3.456 0 2)
 "3.45"
 )


(define (string-_-starts? char=?)
  (lambda (str substr)
    (let ((strlen (string-length str))
	  (sublen (string-length substr)))
      (let lp ((stri 0))
	(let sublp ((subi 0))
	  (if (< subi sublen)
	      (let ((stri* (+ stri subi)))
		(if (< stri* strlen)
		    (if (char=? (string-ref str stri*)
				(string-ref substr subi))
			(sublp (inc subi))
			;; the only change versus string-_-contains?:
			#f)
		    ;; (lp (inc stri)) would be same here:
		    #f))
	      #t))))))

(define string-starts? (string-_-starts? char=?))
(define string-starts-ci? (string-_-starts? char-ci=?))

(define (string-_-contains? char=?)
  (lambda (str substr)
    (let ((strlen (string-length str))
	  (sublen (string-length substr)))
      (let lp ((stri 0))
	(let sublp ((subi 0))
	  (if (< subi sublen)
	      (let ((stri* (+ stri subi)))
		(if (< stri* strlen)
		    (if (char=? (string-ref str stri*)
				(string-ref substr subi))
			(sublp (inc subi))
			(lp (inc stri)))
		    #f))
	      #t))))))

(define string-contains? (string-_-contains? char=?))
(define string-contains-ci? (string-_-contains? char-ci=?))

(TEST
 > (define (test a b)
     (list (string-starts? a b)
	   (string-contains? a b)))
 > (test "" "")
 (#t #t)
 > (test "foo" "")
 (#t #t)
 > (test "foo" "bar")
 (#f #f)
 > (test "foo" "far")
 (#f #f)
 > (test "foo" "fa")
 (#f #f)
 > (test "foo" "f")
 (#t #t)
 > (test "foo" "for")
 (#f #f)
 > (test "foo" "fo")
 (#t #t)
 > (test "foo" "foo")
 (#t #t)
 > (test "foo" "oo")
 (#f #t)
 > (test "foo" "oox")
 (#f #f)
 > (test "foo" "foof")
 (#f #f)
 > (test "foo" "Oo")
 (#f #f)
 > (string-contains-ci? "foo" "Oo")
 #t
 > (string-starts? "foo" "Fo")
 #f
 > (string-starts-ci? "foo" "Fo")
 #t
 )


;; 1 like 'only split once'
(define-typed (string-split-1 str #((either char? procedure?) val-or-pred))
  (let ((len (string-length str))
	(pred (if (procedure? val-or-pred)
		  val-or-pred
		  (lambda (c)
		    (eq? c val-or-pred)))))
    (let lp ((i 0))
      (if (< i len)
	  (if (pred (string-ref str i))
	      (values (substring str 0 i)
		      (substring str i len))
	      (lp (inc i)))
	  (values str "")))))

(TEST
 > (values->vector (string-split-1 "ab  c d" char-whitespace?))
 #("ab" "  c d")
 > (values->vector (string-split-1 "foo?q=1" #\?))
 #("foo" "?q=1")
 > (values->vector (string-split-1 "foo?" #\?))
 #("foo" "?")
 > (values->vector (string-split-1 "foo" #\?))
 #("foo" "")
 )

(define string-reverse
  ;;XX bah
  (compose* list->string reverse string->list))

;; dirname that gives empty string for root. yeah, remembering now
(define (dirname* str)
  ;;XX woah super efficiency and anything
  (chain str
	 (string->list)
	 (list-trim-right (cut char=? <> #\/))
	 (reverse)
	 (list->string)
	 (string-split-1 #\/)
	 (snd)
	 (string->list)
	 (reverse)
	 (list-trim-right (cut char=? <> #\/))
	 (list->string)))

(TEST
 > (dirname* "/foo")
 ""
 > (dirname* "/foo/bar")
 "/foo"
 > (dirname* "/foo/bar/")
 "/foo/bar" ;;  XXX for the web. not otherwise?
 > (dirname* "//foo")
 ""
 > (dirname* "//foo//bar")
 "//foo"
 )
