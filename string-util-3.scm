;; also requires |define.|
;; Hm actually perhaps it doesn't.
;; Well or maybe it does. |for| as macro, then .for for the underlying?

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

