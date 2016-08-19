
(require cj-env
	 dot-oo
	 (unclean make-unclean-string)
	 (list-util let-pair))

(export string-contains-char?
	make-replace-substring
	replace-substring-error
	string.replace-substring
	string.replace-substring-ci
	string.replace-substrings
	string.replace-substrings-ci
	string.maybe-replace-substring
	string.maybe-replace-substring-ci
	string.maybe-replace-substrings
	string.maybe-replace-substrings-ci
	string.drop
	string.take
	string.any
	list.string-reverse
	substring*)


;; (define-macro* (for var seq . body)
;;   `(.for seq
;; 	 (lambda (,var return)
;; 	   ,@body)))


(define (string-contains-char? str pred)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (and (< i len)
	   (or (pred (string-ref str i))
	       (lp (inc i)))))))

(TEST
 > (string-contains-char? "Hello" char-newline?)
 #f
 > (string-contains-char? "Hello\n" char-newline?)
 #t
 )


;; The name string-replace is already used in srfi-13;
;; string-substitute doesn't sound right.

(define (make-replace-substring string-contains invalid all?)
  (named rec
	 (lambda (nomatch)
	   (lambda (s substr withstr)
	     (if (string-empty? substr)
		 (invalid s)
		 (cond ((string-contains s substr)
			=> (lambda (pos)
			     ;; XXX not so efficient, O(n^2)! Work on lists instead?...
			     (string-append
			      (substring s 0 pos)
			      withstr
			      (let ((s* (substring s
						   (+ pos (string-length substr))
						   (string-length s))))
				(if all?
				    ((rec identity) s* substr withstr)
				    s*)))))
		       (else
			(nomatch s))))))))

(define (replace-substring-error v)
  (error "can't replace empty substrings"))

(define. string.replace-substring
  ((make-replace-substring string-contains replace-substring-error #f) identity))
(define. string.replace-substring-ci
  ((make-replace-substring string-contains-ci replace-substring-error #f) identity))
(define. string.replace-substrings
  ((make-replace-substring string-contains replace-substring-error #t) identity))
(define. string.replace-substrings-ci
  ((make-replace-substring string-contains-ci replace-substring-error #t) identity))

(define. string.maybe-replace-substring
  ((make-replace-substring string-contains replace-substring-error #f) false/1))
(define. string.maybe-replace-substring-ci
  ((make-replace-substring string-contains-ci replace-substring-error #f) false/1))
(define. string.maybe-replace-substrings
  ((make-replace-substring string-contains replace-substring-error #t) false/1))
(define. string.maybe-replace-substrings-ci
  ((make-replace-substring string-contains-ci replace-substring-error #t) false/1))


(TEST
 > (%try-error (string.replace-substrings "FooBar" "" "a"))
 #(error "can't replace empty substrings")
 > (string.replace-substrings "FooBar" "abc" "00")
 "FooBar"
 > (string.maybe-replace-substrings "FooBar" "abc" "00")
 #f
 > (string.replace-substrings "FooBar" "oo" "")
 "FBar"
 > (string.replace-substring "FooBar" "oo" "00")
 "F00Bar"
 > (string.replace-substring "FooBar" "oo" "00B")
 "F00BBar"
 > (string.replace-substring "FooBar" "ooB" "00")
 "F00ar"
 > (string.replace-substring "FooBar" "oob" "00")
 "FooBar"
 > (string.replace-substring-ci "FooBarFooBar" "oob" "00")
 "F00arFooBar"
 > (string.replace-substrings-ci "FooBarFooBar" "oob" "00")
 "F00arF00ar"
 > (string.replace-substring "FooBar" "o" "00")
 "F00oBar"
 > (string.replace-substrings "FooBar" "o" "00")
 "F0000Bar")

(define. (string.drop str #(natural0? n))
  (let ((len (string-length str)))
    (substring str n len)))

(TEST
 > (.drop "abc" 0)
 "abc"
 > (.drop "abc" 2)
 "c"
 > (.drop "abc" 3)
 ""
 ;; > (.drop "abc" 4)
 ;; *** ERROR IN (console)@9.1 -- (Argument 2) Out of range
 )

(define. (string.take str #(natural0? n))
  (substring str 0 n))

(TEST
 > (.take "abc" 0)
 ""
 > (.take "abc" 2)
 "ab"
 > (.take "abc" 3)
 "abc"
 ;; > (.take "abc" 4)
 ;; *** ERROR IN (console)@9.1 -- (Argument 2) Out of range
 )

(define. (string.any str pred)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (and (< i len)
	   (or (pred (string-ref str i))
	       (lp (inc i)))))))

(TEST
 > (string.any "foo" char-whitespace?)
 #f
 > (string.any "foo " char-whitespace?)
 #t
 > (string.any " " char-whitespace?)
 #t
 > (string.any "" char-whitespace?)
 #f)


;; (should this be in a list or string lib? string since list is the
;; default type in lisps.)

(define. (list.string-reverse l #!optional (len (length l)))
  (let ((out (make-unclean-string len)))
    (let lp ((i (dec len))
	     (l l))
      (if (negative? i)
	  out
	  (let-pair ((c l*) l)
		    (string-set! out i c)
		    (lp (dec i) l*))))))

(TEST
 > (.string-reverse '(#\c #\b #\a))
 "abc"
 > (.string-reverse '(#\c #\b #\a) 2)
 "bc")



(define (substring* str i)
  (substring str i (string-length str)))

(TEST
 > (substring* "foo-bar" 3)
 "-bar")

