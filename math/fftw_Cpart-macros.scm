
(define-macro* (define-!fftwcfftwc._ op)
  (let* ((op-string (symbol->string* op))
	 (op-char (string-ref op-string 0)))
    ;;(define-macro-symbol-replace-_-with R op-char)
    (define T (symbol-replace-_-with/ op-char))
    (quasiquote-source
     (begin
       (define (,(T '!fftwcfftwc._@) res x y)
	 (##c-code ,(string-append
		     "*(FFTWc_BODY(___ARG1)) = *(FFTWc_BODY(___ARG2)) "
		     op-string
		     " *(FFTWc_BODY(___ARG3));")
		   res x y)
	 (void))
       (define-typed (,(T '!fftwcfftwc._) #(fftwc? res) #(fftwc? x) #(fftwc? y))
	 (,(T '!fftwcfftwc._@) res x y))))))


(define-macro* (define-fftw-matrix-ops c-or-r Ctype)
  ;; (define-macro-symbol-replace-_-with R c-or-r) not working/ quasiquote-source
  (define T (symbol-replace-_-with/ c-or-r))
  (assert*
   string? Ctype
   (lambda (Ctype)
     (quasiquote-source
      (begin
	;; accessors: *.ref*
	(define (,(T '!fftwM_.ref@) res m i)
	  ,(let ((FFTW__BODY (string-append "FFTW" (string* c-or-r) "_BODY")))
	     `(##c-code ,(string-append
			  "*"FFTW__BODY"(___ARG1)"
			  "="
			  "___CAST("Ctype"*,___ARG2)[___INT(___ARG3)];cjabort();")
			res
			(,(T 'fftwM_.ptr) m)
			i)))
	(define-typed (,(T '!fftwM_.ref)
		       #(,(T 'fftw_?) res)
		       #(,(T 'fftwM_?) m)
		       #(size0? i0)
		       #(size0? i1))
	  (letv ((size0 size1) (,(T 'fftwM_.sizes) m))
		(assert (< i0 size0))
		(assert (< i1 size1))
		(let ((i (+ (* i0 size1) i1)))
		  (assert (size0? i))
		  (,(T '!fftwM_.ref@) res m i))))
	(define. (,(T 'fftwM_.ref) m i0 i1)
	  (let ((res (,(T '@make-fftw_))))
	    (,(T '!fftwM_.ref) res m i0 i1)
	    (,(T 'fftw_->) res)))
	(define. (,(T 'fftwM_.ref*) m i0 i1)
	  (,(T 'fftwM_.ref) m (dec i0) (dec i1)))

	;; *.copy-to-fftw!
	(define (,(T 'fftwM_M_.copy-to-fftw!) fm m)
	  (let-values (((s0* s1*) (,(T 'fftwM_.sizes) fm))
		       ((s0 s1) (,(T 'M_.sizes) m)))
	    (assert (= s0 s0*))
	    (assert (= s1 s1*))
	    (let ((p (,(T 'fftwM_.ptr) fm))
		  (data (,(T 'M_.data) m)))
	      (for..< (row 0 s0)
		      ;; *have* to do address taking in C, since body can move
		      ;; while in Scheme
		      (let ((i (* row s1))
			    (v (vector-ref data row)))
			(assert (fixnum? i))
			(assert (= (,(T 'V_.size) v) s1))
			(##c-code
			 ,(string-append
			   "memcpy(&(___CAST("Ctype"*,
                                        ___ARG1)[___INT(___ARG2)]),
                              ___BODY(___ARG3),
                              ___INT(___ARG4) * sizeof("Ctype"));")
			 p i (,(T 'V_.data) v) s1))))))
	(define (,(T 'M_fftwM_.copy-from-fftw!) m fm)
	  ;; heh big part the same; basically, iterator abstraction wanted?
	  (let-values (((s0 s1) (,(T 'fftwM_.sizes) fm))
		       ((s0* s1*) (,(T 'M_.sizes) m)))
	    (assert (= s0 s0*))
	    (assert (= s1 s1*))
	    (let ((p (,(T 'fftwM_.ptr) fm))
		  (data (,(T 'M_.data) m)))
	      (for..< (row 0 s0)
		      ;; *have* to do address taking in C, since body can move
		      ;; while in Scheme
		      (let ((i (* row s1))
			    (v (vector-ref data row)))
			(assert (fixnum? i))
			(assert (= (,(T 'V_.size) v) s1))
			;; WOW it's *identical* except for this form:
			(##c-code
			 ,(string-append
			   "memcpy(___BODY(___ARG1),
                                   &(___CAST("Ctype"*,___ARG2)[___INT(___ARG3)]),
                                   ___INT(___ARG4) * sizeof("Ctype"));")
			 (,(T 'V_.data) v) p i s1)))))))))))

