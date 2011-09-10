(define (vector-equal? equal? a b)
  (let ((la (vector-length a))
	(lb (vector-length b)))
    (and (= la lb)
	 (let lp ((i 0))
	   (if (< i la)
	       (and (equal? (vector-ref a i)
			    (vector-ref b i))
		    (lp (inc i)))
	       #t)))))

(define (source-equal? a b)
  (let ((a* (source-code a))
	(b* (source-code b)))
    (or (eq? a* b*)
	(cond ((number? a*)
	       (and (number? b*)
		    (= a* b*)))
	      ((string? a*)
	       (and (string? b*)
		    (string=? a* b*)))
	      ;; don't examine symbols more closely than eq?;
	      ((pair? a*)
	       (and (pair? b*)
		    (source-equal? (car a*) (car b*))
		    (source-equal? (cdr a*) (cdr b*))))
	      ((vector? a*)
	       (and (vector? b*)
		    (vector-equal? source-equal? a* b*)))
	      ;; XXX boxes? and more?
	      ((symbol? a*)
	       #f)
	      (else
	       (error "source-equal?: unknown type of:" (cj-desourcify a*)))))))

(TEST
 > (source-equal? 'a 'a)
 #t
 > (source-equal? 'a 'b)
 #f
 > (source-equal? '#(a b) '#(a b))
 #t
 > (source-equal? '#(a b) '#(a b c))
 #f
 > (source-equal? '#(a b) '#(a c))
 #f
 > (source-equal? '#(a b) '(a b))
 #f
 > (source-equal? '(a . b) '(a . b))
 #t
 > (source-equal? '(a . b) '(a b))
 #f
 > (source-equal? '(a . #(b)) '(a . #(b)))
 #t
 > (source-equal? '(a . #(b)) '(a . #()))
 #f
 > (source-equal? '(a . #("a")) '(a . #("a")))
 #t
 > (source-equal? '(a . #("a")) '(a . #("b")))
 #f
 > (source-equal? '(a . #("a")) '(a . #(#f)))
 #f
 )

(define-macro* (qq form)
  `(u8vector->object ',(object->u8vector form)))

(TEST
 > (source-equal? 'a (qq a))
 #t
 > (source-equal? 'a (qq b))
 #f
 > (source-equal? '#(a b) (qq #(a b)))
 #t
 > (source-equal? '#(a b) (qq #(a c)))
 #f
 > (source-equal? '(a . b) (qq (a . b)))
 #t
 > (source-equal? '(a . b) (qq (a b)))
 #f
 > (source-equal? '(a . #(b)) (qq (a . #(b))))
 #t
 > (source-equal? '(a . #(b)) (qq (a . #())))
 #f
 > (source-equal? '(a . #("a")) (qq (a . #("a"))))
 #t
 > (equal? '(a . #("a")) (qq (a . #("a"))))
 #f
 )

(define-macro* (force-source-code vars . body)
  `(let ,(map (lambda (v)
		`(,v (source-code ,v)))
	      (source-code vars))
     ,@body))

(TEST
 > (expansion#force-source-code (x y) foo)
 (let ((x (source-code x)) (y (source-code y))) foo)
 )
