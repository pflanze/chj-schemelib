;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 define-macro-star
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
;;  > (define (t-foo f)
;;      (local-TEST*
;;       > (f 10)
;;       1234
;;       > (f 11)
;;       12355))
;;  > (%test (t-foo foo))
;;  > (%test (t-foo foo*)))

;; or, if parametrization isn't the motivation for it:
;; (TEST
;;  > (parameterize
;;     ((current-manadb (empty-manadb/time 1)))
;;     (local-TEST 
;;      > 1
;;      2)))


(define (TEST-expand test-check)
  (named self
	 (lambda (l)
	   (define (sideeffect e rest)
	     (source-error e "local-TEST: side-effects not currently supported"))
	   (mcase l
		  (null?
		   '())
		  (`(> `e)
		   (sideeffect e '()))
		  (`(> `e > . `_rest)
		   (sideeffect e (cddr (source-code l))))
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


;; run a test separately
(define (run-test t)
  (if TEST:running
      (error "already running a test suite")
      (begin
	(set! TEST:count-success 0)
	(set! TEST:count-fail 0)
	(set! TEST:running #t)
	(t)
	(print (list TEST:count-success " success(es), "
		     TEST:count-fail " failure(s)" "\n"))
	(set! TEST:running #f))))

(define-macro* (%test e)
  `(begin
     (begin (display "test form: ") (write ',e) (newline))
     (,e)))

