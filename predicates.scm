(require test
	 srfi-1
	 (list-util improper-fold-right)
	 (char-util char-one-of?/)
	 cj-functional
	 (cj-env-2 C)
	 (string-util-4 string-empty?
			string-every)
	 (improper-list improper-any))


(export forced ;; rename to possibly-promise-of ?
	any? true/1
	false/2
	false? ;; == not
	true?
	true
	inexact-real?
	exact-real?
	exact-number?
	pair-or-null?
	pair-with-car
	nonempty-string?
	improper*-map/tail ;; XX move
	improper*-map ;; dito
	string-of
	improper-every  ;; XX move
	improper-list-of ;; hmm
	char-one-of ;; move to char lib?
	perhaps-source-of ;; XX rename to possibly-source-of ?
	source-of
	perhaps-source*-of ;; dito
	source*-of
	length-=
	length-is ;; see also list-of/length  -- rename to list-of-length ?
	list-of-length
	0..1? ;; see also rgb:0..1?
	in-signed-range?)


(define (forced pred)
  (lambda (v)
    (pred (force v))))

(define (true/1 v)
  #t)
(define (false/2 a b)
  #f)

(define any? true/1)

(define false? not) ;; so as to be able to use "false." as OO prefix

;; maybe also, since at it:
(define (true? x) (eq? x #t)) ;; dangerous to mistake?
(define (true x)
  ;; any kind of true; identity
  (not (not x)))

(define inexact-real? (both real? inexact?))

(define exact-real? (both real? exact?))

(define exact-number? (both number? exact?))

(TEST
 > (exact-real? 3+2i)
 #f
 > (exact-number? 3+2i)
 #t)


(define (pair-or-null? v)
  (or (pair? v)
      (null? v)))

(define (pair-with-car pred)
  (lambda (v)
    (and (pair? v)
	 (pred (car v)))))

;; btw should probably move predicates stuff from cj-functional here

(define nonempty-string?
  (both string?
	(complement string-empty?)))

;; improper->proper-map

(define (improper*-map/tail fn v tail)
  (improper-fold-right (lambda (a r)
			 (cons (fn a) r))
		       tail
		       v))

(define improper*-map (C improper*-map/tail _ _ '()))

(TEST
 > (improper*-map true? '("" . ""))
 (#f #f))


(define (string-of pred)
  (lambda (v)
    (and (string? v)
	 (string-every pred v))))

(TEST
 > (map (string-of char-alphanumeric?) '(foo "" " " "foo" "foo bar" "foo:" "Foo_"))
 (#f #t #f #t #f #f #t))


(define (improper-every pred v)
  (cond ((pair? v)
	 (and (pred (car v))
	      (improper-every pred (cdr v))))
	((null? v)
	 #t)
	(else
	 (pred v))))

(define (improper-list-of pred)
  (C improper-every pred _))

(TEST
 > (map (improper-list-of (string-of char-alphanumeric?))
	'("foo" ("a" "b") ("a" . "b") ("a" . b) ("a" ("b"))))
 (#t #t #t #f #f))


(define char-one-of char-one-of?/)


(define (perhaps-source-of pred)
  (lambda (v)
    (pred (source-code v))))

(define (source-of pred)
  (lambda (v)
    (and (source? v)
	 (pred (source-code v)))))

(define (perhaps-source*-of pred)
  (lambda (v)
    (pred (cj-desourcify v))))

(define (source*-of pred)
  (lambda (v)
    (and (source? v)
	 (pred (cj-desourcify v)))))


(define (length-= l len)
  (if (null? l)
      (zero? len)
      (if (zero? len)
	  #f
	  (length-= (cdr l) (dec len)))))

(TEST
 > (length-= '() 0)
 #t
 > (length-= '() 1)
 #f
 > (length-= '(a) 1)
 #t
 > (length-= '(a) 0)
 #f
 > (length-= '(a b) 2)
 #t)


;; see also list-of/length
(define (length-is len)
  (lambda (l)
    (length-= l len)))


;; XX base on length-= for simplification?
(define (list-of-length n)
  (lambda (v)
    (let lp ((v v)
	     (len 0))
      (cond ((pair? v)
	     (if (< len n)
		 (lp (cdr v) (inc len))
		 #f))
	    ((null? v)
	     (= len n))
	    (else
	     ;; not a list
	     #f)))))

(TEST
 > (def vals '(() (a) (a b) (a b c) (a b . c) a))
 > (map (list-of-length 2) vals)
 (#f #f #t #f #f #f)
 > (map (list-of-length 0) vals)
 (#t #f #f #f #f #f)
 > (map (list-of-length 4) vals)
 (#f #f #f #f #f #f))


(define (0..1? v)
  (and (real? v)
       (<= 0 v)
       (<= v 1)))

;; also see rgb:0..1? which accepts 1 milli unit of change.



(define (in-signed-range? wordsize-bits v)
  (let ((half (expt 2 (dec wordsize-bits))))
    (and (<= (- half) v)
	 (< v half))))

(TEST
 > (define (test basenum v)
     (list (in-signed-range? 8 v)
	   (number->string (+ basenum v) 2)))
 > (test (expt 2 16) -1)
 (#t "1111111111111111")
 ;;   1234567812345678
 > (test (expt 2 16) -126)
 (#t "1111111110000010")
 > (test (expt 2 16) -127)
 (#t "1111111110000001")
 > (test (expt 2 16) -128)
 (#t "1111111110000000")
 > (test (expt 2 16) -129)
 (#f "1111111101111111")
 > (test (expt 2 15) 0)
 (#t "1000000000000000")
 > (test (expt 2 15) 127)
 (#t "1000000001111111")
 > (test (expt 2 15) 128)
 (#f "1000000010000000")
 ;;   1234567812345678
 )

