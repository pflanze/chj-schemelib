;;lib
(define (1st/2 f)
  (lambda (a _b);;  . _rest ?
    (f a)))


;; propagate from Scheme #f to optional 'failures'

;; current-fail is called in the context of the failure (hence could
;; continue or capture the continuation), with the serialized source
;; code of the form that failed and its value as the arguments.

(define current-fail (make-parameter false/2))

(define (with-fail-handler handler thunk)
  (parameterize ((current-fail (lambda (vec rest)
				 (handler (u8vector->object vec) rest))))
		(thunk)))

(define (with-fail-catcher handler thunk)
  (continuation-capture
   (lambda (return)
     (parameterize ((current-fail
		     (lambda (vec rest)
		       (continuation-graft
			return
			handler
			(u8vector->object vec)
			rest))))
		   (thunk)))))


(define (fail:and-expand If Or forms)
  (fold-right/last
   (lambda (form tail)
     (with-gensym
      V
      `(##let ((,V ,form))
	      (,If ,V
		   ,tail
		   ((current-fail) ',(object->u8vector form) ,V)))))
   (lambda (form tail)
     (assert (null? tail))
     (with-gensym
      V
      `(##let ((,V ,form))
	      ,(Or V
		   `((current-fail) ',(object->u8vector form) ,V)))))
   '()
   forms))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (fail:and-expand 'if
		    (lambda (t f)
		      `(or ,t ,f))
		    '("a" "b"))
 (##let ((GEN:V-2236 "a"))
	(if GEN:V-2236
	    (##let ((GEN:V-2258 "b"))
		   (or GEN:V-2258 ((current-fail) '#u8(17 98) GEN:V-2258)))
	    ((current-fail) '#u8(17 97) GEN:V-2236))))



(define-struct* failure
  stack)

(define-macro* (fail:if t a b)
  `(##if (##or (##not ,t) (failure? ,t))
	 ,b
	 ,a))

(define-macro* (fail:and . forms)
  (fail:and-expand 'fail:if
		   (lambda (t f)
		     `(fail:if ,t ,t ,f))
		   forms))


(define (with-failures thunk)
  (with-fail-handler (lambda (v rest)
		       (if rest
			   (failure-stack-update
			    rest (lambda (vs)
				   (cons v vs)))
			   (failure (cons v '()))))
		     thunk))

(TEST
 > (define (tests)
     (list (fail:and 1 2)
	   (fail:and 1 #f)
	   (fail:and #f 2)
	   (fail:and (even? 4) (even? 5))
	   (fail:and (even? 7) (even? 8))
	   (fail:and (even? 9) (even? 11))
	   (fail:and (even? 10) (even? 12))
	   (fail:and (even? -2)
		     (fail:and (even? 0)
			       (odd? 0)))))
 > (tests)
 (2 #f #f #f #f #f #t #f)
 > (cj-desourcify (with-failures tests))
 (2
  #(failure (#f))
  #(failure (#f))
  #(failure ((even? 5)))
  #(failure ((even? 7)))
  #(failure ((even? 9)))
  #t
  #(failure ((fail:and (even? 0)
		       (odd? 0))
	     (odd? 0)))))


;; ------------------------------------------------------------------
;; Alternative variants, don't use for consistency!

(IF #f
    (begin

      (define-macro* (fail:_and . forms)
	(fail:and-expand 'if
			 (lambda (t f)
			   `(or ,t ,f))
			 forms))

      (define-macro* (fail:And . forms)
	(fail:and-expand 'If
			 (lambda (t f)
			   `(If ,t #t ,f))
			 forms))



      (TEST
       > (fail:_and 1 2)
       2
       > (with-fail-catcher (1st/2 cj-desourcify) (& (fail:_and (even? 4) (even? 5))))
       (even? 5)
       > (%try-error (fail:And 1 2))
       #(error "If: expecting boolean, got:" 1)
       > (%try-error (fail:And #t 2))
       #(error "If: expecting boolean, got:" 2)
       > (fail:And #t (even? 6))
       #t
       > (fail:And #t (even? 7))
       #f
       > (with-fail-catcher (1st/2 cj-desourcify) (& (fail:And #t (even? 7))))
       (even? 7)
       > (with-fail-catcher (1st/2 cj-desourcify) (& (fail:And (even? 5) (even? 7))))
       (even? 5)
       )


      ;; |iF|, accept #t as true, treat everything else as false?

      (define-macro* (iF t a b)
	`(##if (##eq? ,t #t)
	       ,a
	       ,b))

      (TEST
       > (iF #t 'y 'n)
       y
       > (iF 1 'y 'n)
       n
       > (iF #f 'y 'n)
       n)


      (define-macro* (fail:anD . forms)
	(fail:and-expand 'iF
			 (lambda (t f)
			   `(iF ,t #t ,f))
			 forms))

      (TEST
       > (fail:anD 1 2)
       #f ;; because the default handler is false/1
       > (fail:anD 1 #t)
       #f
       > (fail:anD (even? 4) (even? 6))
       #t
       > (with-fail-handler (1st/2 cj-desourcify) (& (fail:anD (even? 4) (even? 7))))
       (even? 7)
       > (with-fail-handler (1st/2 cj-desourcify) (& (fail:anD (even? 5) (even? 7))))
       (even? 5))))


;; ------------------------------------------------------------------
;; supporting library

(define (fail:every pred l)
  (let lp ((l l))
    (if (null? l)
	#t
	(let-pair ((a r) l)
		  (let ((v (pred a)))
		    (fail:if v
			     (lp r)
			     ;; XXX hm return a? since, what source
			     ;; could I show here? !
			     ((current-fail) a v)))))))

