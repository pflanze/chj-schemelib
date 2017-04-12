
(define (.type v #!optional show-parameters?)
  ;; now now how: extensible, well, isn't, right?.
  ;; NOT fast btw.; and not 'R5RS conform'
  (let ((show (lambda (t . fns)
		(if show-parameters?
		    (cons (symbol-append 'make- t)
			  (map (lambda (fn)
				 (fn v))
			       fns))
		    t))))
    (mcase v
	   (number?
	    (mcase v
		   ((both integer? exact?)
		    (show 'integer (mcase-lambda
				    (fixnum? 'fix)
				    (bignum? 'big))))
		   (rational?
		    (mcase v
			   (exact? 'fractional)
			   (else 'real)))
		   (real? 'real)
		   (complex?
		    ;; (be careful, just ~the rest (what would
		    ;; non-complex numbers be that don't satisfy the
		    ;; complex? predicate?))
		    `(complex ,(.type (real-part v))
			      ,(.type (imag-part v))))))
	   (string? (show 'string string-length))
	   (u8vector? (show 'u8vector u8vector-length))
	   (Vr? (show 'Vr .size))
	   (Vc? (show 'Vc .size))
	   (Vi? (show 'Vi .size))
	   (Vs? (show 'Vs .size))
	   (Vb? (show 'Vb .size))
	   (Mr? (show 'Mr .size0 .size1))
	   (Mc? (show 'Mc .size0 .size1))
	   (vector? (show 'vector vector-length)))))

(TEST
 > (.type 1)
 integer ;; separate into fixnum and bignum ?
 > (.type 5.)
 real ;; NOT integer unlike what integer? says, ok?
 > (.type 1.2)
 real
 > (.type 1.23234234)
 real
 > (.type 1/2)
 fractional
 > (.type 5+1i)
 (complex integer integer)
 > (.type 5.+1i)
 (complex real integer)
 > (.type 5.+1.i)
 (complex real real)
 > (.type 5.+1/2i)
 (complex real fractional)
 > (.type '#(1 2))
 vector
 )


;; type 'with runtime info too' [wl  parametrized types  wl  whatever]
(define (.info v)
  (.type v #t))

(TEST
 > (.info 5)
 (make-integer fix) ;; yay
 > (.info 5+1/2i)
 (complex integer fractional) ;; hm still; well ok?
 > (.info '#(1 2))
 (make-vector 2)
 ;; ^ without saying anything about what it contains; which is ok,
 ;; it's not a homogenous vector. Da.
 > (.info '#u8(1 2))
 (make-u8vector 2)
 > (.info (Vc 1))
 (make-Vc 1)
 > (.info (Mr (Vr 1) (Vr 2)))
 (make-Mr 2 1)
 )

