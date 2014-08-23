;; Provide compatibility with Gambit 4.2.8-1.1 in Debian

(insert-result-of
 (with-exception-catcher 
  (lambda (e) 
    (cond ((wrong-number-of-arguments-exception? e)
	   `(begin
	      (define warned-read-subu8vector? #f)
	      (define (read-subu8vector v start end #!optional port need)
		(or (u8vector? v) (error "not an u8vector:" v))
		(or (and (##fixnum? start) (not (negative? start))) (error "not a non-negative fixnum:" start))
		(or (and (##fixnum? end) (not (negative? end))) (error "not a non-negative fixnum:" end))
		(if need
		    (begin
		      ;; XXX hm can we simply ignore need?
		      (if (not warned-read-subu8vector?)
			  (begin
			    (display "warning (compat.scm): ignoring need argument for read-subu8vector calls\n"
				     (current-error-port))
			    (set! warned-read-subu8vector? #t)))))
		(if port
		    (begin
		      (or (port? port) (error "not a port:" port))
		      (##read-subu8vector v start end port))
		    (##read-subu8vector v start end)))))
	  ((type-exception? e)
	   ;; nothing to change
	   `(begin))
	  (else
	   (raise e))))
  (lambda ()
    (read-subu8vector #f 0 10 'port 'need))))

(insert-result-of
 (with-exception-catcher 
  (lambda (e) 
    (cond ((unbound-global-exception? e)
	   `(begin
	      (define (random-u8vector len)
		(let ((v (##make-u8vector len)))
		  (for..< (i 0 len)
			  (u8vector-set! v i (random-integer 256)))
		  v))))
	  (else
	   (raise e))))
  (lambda ()
    (random-u8vector 1)
    `(begin))))

(insert-result-of
 (with-exception-catcher 
  (lambda (e) 
    (cond ((unbound-global-exception? e)
	   `(begin
	      (define (open-output-process s)
		(open-process (append (list stdout-redirection: #f
					    stdin-redirection: #t)
				      s)))))
	  (else
	   (raise e))))
  (lambda ()
    (eval 'open-output-process) 
    `(begin))))

