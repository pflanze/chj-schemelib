;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 define-macro-star
	 (cj-match mcase)
	 (cj-env named)
	 (cj-source cj-sourcify-deep source-location))

(export (macro local-TEST)
	;; and for parametrized ones:
	(macro local-TEST*)
	run-test
	(macro %test))


;; Variant of lib/test.scm's TEST that works in local function scopes.

;; Notes:

;; - does not move code to file, thus bloating code base

;; - only supports form + result pairs, no void statements,
;; TEST:equal? or other features

;; See TEST:conv for what else would be needed (currently
;; maybe-namespace-form).

;; Example:

;; (TEST
;;  > (parameterize
;;     ((current-foo 123))
;;     (local-TEST 
;;      > 1
;;      2)))

;; or, if parametrization isn't the motivation for it:

;; (TEST
;;  > (define (t-foo f)
;;      (local-TEST*
;;       > (f 10)
;;       1234
;;       > (f 11)
;;       12355))
;;  > (%test (t-foo foo))
;;  > (%test (t-foo foo*)))


;; (XX vs. expansion function in test.scm?)

(define (TEST-expand test-check)
  (named self
	 (lambda (l)
	   (define (sideeffect e rest)
	     (cons e (self rest)))
	   (mcase l
		  (null?
		   '())
		  (`(> `e)
		   (sideeffect e '()))
		  (`(> `e > . `rest)
		   (sideeffect e (cons '> rest)))
		  (`(> `e `res . `rest)
		   (cons `(,test-check ,e ,res)
			 (self rest)))))))

(TEST
 > ((TEST-expand 'test-equal?) '())
 ()
 > ((TEST-expand 'test-equal?) '(> A B > c d))
 ((test-equal? A B) (test-equal? c d)))

(define-macro* (test-check e res)
  (cj-sourcify-deep
   `(TEST:check (let ((repl-result-history-ref TEST:repl-result-history-ref))
		  ;; ,@(if maybe-namespace-form
		  ;;       (list maybe-namespace-form)
		  ;;       (list))
		  ,e)
		',res
		',(source-location e)
		;; TEST:equal?
		equal?)
   e))

(define-macro* (local-TEST* . args)
  `(lambda ()
     ,@((TEST-expand 'test-check) args)))

(define-macro* (local-TEST . args)
  `(begin
     ,@((TEST-expand 'test-check) args)))


(TEST
 > (expansion#local-TEST > 1 2)
 (begin (test-check 1 2))
 > (expansion#local-TEST* > 1 2)
 (lambda () (test-check 1 2))
 > (expansion#local-TEST* > (def f 1))
 (lambda () (def f 1))
 > (expansion#local-TEST > (def f 1) > f 1)
 (begin (def f 1) (test-check f 1)))



;; run a test separately
(define (run-test t)
  (if (TEST:running)
      (error "already running a test suite")
      (parameterize
       ((TEST:running #t)
	(TEST:count-success 0)
	(TEST:count-fail 0))
       (t)
       (print (list (TEST:count-success) " success(es), "
		    (TEST:count-fail) " failure(s)" "\n")))))


(TEST
 > (eq? (local-TEST > 1 1) (void))
 #t)

(TEST
 > (%try-error (run-test (& (local-TEST > 1 1))))
 [error "already running a test suite"]
 > (parameterize ((TEST:running #f))
		 (eq? (run-test (& (local-TEST > 1 1))) (void)))
 ;; prints a superfluous 1 success(es), 0 failure(s)
 #t)


(define-macro* (%test e)
  `(begin
     (begin (display "test form: ") (write ',e) (newline))
     (,e)))

(TEST
 > (define didrunit? #f)
 > (define (t-foo f)
     (local-TEST*
      > (f 10)
      100
      > (begin (set! didrunit? #t) (f 11))
      121))
 > (eq? (%test (t-foo square)) (void))
 #t
 > didrunit?
 #t)


