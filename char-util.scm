(define (char=?/ c)
  (cut char=? <> c))

(define (char-one-of?/ str)
  (let ((strlen (string-length str)))
    (lambda (c)
      (let lp ((i 0))
	(and (< i strlen)
	    (or (char=? c (string-ref str i))
		(lp (inc i))))))))

(TEST
 > ((char-one-of?/ "Halo") #\_)
 #f
 > ((char-one-of?/ "Halo") #\o)
 #t
 > ((char-one-of?/ "Halo") #\O)
 #f
 )


(define (on-char fn)
  (lambda (c)
    (fn (char->integer c))))

(define char-digit?
  (on-char (cut <= (char->integer #\0) <> (char->integer #\9))))

(define char-alpha-lc?
  (on-char (cut <= (char->integer #\a) <> (char->integer #\z))))

(define char-alpha-uc?
  (on-char (cut <= (char->integer #\A) <> (char->integer #\Z))))

(define char-alpha?
  (either char-alpha-lc? char-alpha-uc?))

(define char-alphanumeric?
  (either char-digit? char-alpha? (char=?/ #\_)))

(TEST
 > (every char-alphanumeric? (string->list "abc "))
 #f
 > (every char-alphanumeric? (string->list "abc_123_A"))
 #t
 )

(define (char-in-range? fromchar tochar)
  (let ((from (char->integer fromchar))
	(to (char->integer tochar)))
    (lambda (v)
      (<= from (char->integer v) to))))

(define char-hexdigit?
  (either char-digit?
	  (char-in-range? #\a #\f)
	  (char-in-range? #\A #\F)))

(TEST
 > (char-hexdigit? #\x)
 #f
 > (char-hexdigit? #\f)
 #t
 > (char-hexdigit? #\F)
 #t
 > (char-hexdigit? #\9)
 #t
 )

