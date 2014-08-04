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

(define (make-replace-substring string-contains invalid)
  (named rec
	 (lambda (nomatch)
	   (lambda (s substr withstr all?)
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
				    ((rec identity) s* substr withstr all?)
				    s*)))))
		       (else
			(nomatch s))))))))

(define (replace-substring-error v)
  (error "can't replace empty substrings"))

(define. string.replace-substring
  ((make-replace-substring string-contains replace-substring-error) identity))
(define. string.replace-substring-ci
  ((make-replace-substring string-contains-ci replace-substring-error) identity))

(define. string.maybe-replace-substring
  ((make-replace-substring string-contains replace-substring-error) false/1))
(define. string.maybe-replace-substring-ci
  ((make-replace-substring string-contains-ci replace-substring-error) false/1))


(TEST
 > (%try-error (string.replace-substring "FooBar" "" "a" #t))
 #(error "can't replace empty substrings")
 > (string.replace-substring "FooBar" "abc" "00" #t)
 "FooBar"
 > (string.maybe-replace-substring "FooBar" "abc" "00" #t)
 #f
 > (string.replace-substring "FooBar" "oo" "" #t)
 "FBar"
 > (string.replace-substring "FooBar" "oo" "00" #f)
 "F00Bar"
 > (string.replace-substring "FooBar" "oo" "00B" #f)
 "F00BBar"
 > (string.replace-substring "FooBar" "ooB" "00" #f)
 "F00ar"
 > (string.replace-substring "FooBar" "oob" "00" #f)
 "FooBar"
 > (string.replace-substring-ci "FooBarFooBar" "oob" "00" #f)
 "F00arFooBar"
 > (string.replace-substring-ci "FooBarFooBar" "oob" "00" #t)
 "F00arF00ar"
 > (string.replace-substring "FooBar" "o" "00" #f)
 "F00oBar"
 > (string.replace-substring "FooBar" "o" "00" #t)
 "F0000Bar")

