;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XX move to cj-source-lambda.scm?

'(require)

'(export schemedefinition-arity:template->checker
	schemedefinition-arity:pattern->template
	schemedefinition-arity-checker
	improper-length ;;via (include "improper-length--source.scm")
	safer-apply)

(include "cj-standarddeclares-1--include.scm")


;; tests see cj-source-util-test.scm


(define (schemedefinition-arity:template->checker t)
  (let ((min-count (vector-ref t 1)))
    (case (vector-ref t 0)
      ((at-least)
       (lambda (argslen)
	 (if (>= argslen min-count)
	     'ok
	     'not-enough)))
      ((up-to)
       (let ((max-count (vector-ref t 2)))
	 (lambda (argslen)
	   (if (>= argslen min-count)
	       (if (<= argslen max-count)
		   'ok
		   'too-many)
	       'not-enough))))
      ((exact)
       (lambda (argslen)
	 (cond ((= argslen min-count)
		'ok)
	       ((< argslen min-count)
		'not-enough)
	       (else
		'too-many))))
      (else
       (error "bug")))))


(define (schemedefinition-arity-template-qualifier? v)
  (case v
    ;; XX should probably rename 'exact to 'fixed
    ((at-least up-to exact) #t)
    (else #f)))

(define (schemedefinition-arity:pattern->template lis)
  ;; -> (vector-of schemedefinition-arity-template-qualifier?
  ;;               fixnum-natural0?)

  (define (inc x);; copy from cj-env because of phasing issue
    (+ x 1))
  (let lp ((l (source-code lis))
	   (min-count 0))
    (define (at-least)
      (vector 'at-least min-count))
    (define (up-to max-count)
      (vector 'up-to min-count max-count))

    (define (handle-key l max-count)
      ;; l is the remainder after #!key.  Each key argument
      ;; requires two args.  [and we still don't check for
      ;; correct keys yet, not even for even number of
      ;; arguments in key area]

      (let lp ((l l)
               (max-count max-count))
        (cond ((null? l)
               (up-to max-count))
              ((pair? l)
               ;; *almost* copy from above, hm.
               (let ((a (car l)))
                 (cond ((eq? a #!rest)
                        (at-least))
                       ((eq? a #!optional)
                        (error "#!optional after #!key in argument list:"
                               lis))
                       ((eq? a #!key)
                        (error "more than one #!key in argument list:" lis))
                       (else
                        (lp (cdr l)
                            (+ max-count 2))))))
              (else
               (at-least)))))

    (cond ((pair? l)
	   (let ((a (car l)))
             
	     (cond ((eq? a #!rest)
		    (at-least))
		   ((eq? a #!optional)
		    (let lp ((l (cdr l))
			     (max-count min-count))
		      (cond ((null? l)
			     (up-to max-count))
			    ((pair? l)
			     (let ((a (car l)))
			       (cond ((eq? a #!rest)
				      (at-least))
				     ((eq? a #!optional)
				      (error "more than one #!optional in argument list:"
                                             lis))
				     ((eq? a #!key)
				      (handle-key (cdr l) max-count))
				     (else
				      (lp (cdr l)
					  (inc max-count))))))
			    (else
			     (at-least)))))
		   ((eq? a #!key)
                    (handle-key (cdr l) min-count))
		   (else
		    (lp (cdr l)
			(inc min-count))))))
	  ((null? l)
	   (vector 'exact min-count))
	  (else
	   (at-least)))))

(define (schemedefinition-arity-checker x)
  (schemedefinition-arity:template->checker
   (schemedefinition-arity:pattern->template
    x)))


(include "improper-length--include.scm")

(define (safer-apply template fn args err cont)
  (declare (not optimize-dead-local-variables))
  (let ((len (improper-length args)))
    (if (negative? len)
	(error "got improper list:" args)
	(case ((schemedefinition-arity:template->checker template) len)
	  ((ok) (cont (apply fn args)))
	  ((not-enough) (err "not enough arguments"))
	  ((too-many) (err "too many arguments"))
	  (else (error "bug in safer-apply"))))))

