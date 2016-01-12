
(define false? not) ;; so as to be able to use "false." as OO prefix
(define (anything? x) #t)
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

(define improper*-map (cut improper*-map/tail <> <> '()))

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
  (cut improper-every pred <>))

(TEST
 > (map (improper-list-of (string-of char-alphanumeric?))
	'("foo" ("a" "b") ("a" . "b") ("a" . b) ("a" ("b"))))
 (#t #t #t #f #f))

;; XX move to an improper-srfi-1 ?
(define (improper-any pred v)
  (cond ((pair? v)
	 (or (pred (car v))
	     (improper-any pred (cdr v))))
	((null? v)
	 #f)
	(else
	 (pred v))))

(TEST
 > (improper-any identity '(#f 1 #f))
 1
 > (improper-any identity '2)
 2
 > (improper-any even? 1)
 #f
 > (improper-any even? 2)
 #t
 > (improper-any even? '())
 #f)


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
