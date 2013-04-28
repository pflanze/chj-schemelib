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

