;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 define-macro-star
	 cj-phasing
	 ;; improper-length is included by cj-source-util.scm is
	 ;; included by define-macro-star.scm
	 )


; (define (warn* message . args)
;   (continuation-capture
;    (lambda (cont)
;      )))
;or, 'simpler':
(define-macro* (warn* message . args)
  (let ((loc (source-location stx)))
    `(location-warn ',loc
		    ,message
		    ,@args)))
 

(define-macro* (match* input* . clauses*)
  ;; only supports flat list matching for now
  ;; although including improper lists
  ;; not even anything other than just binding variables.
  ;; also doesn't check for errors in clauses syntax very well.
  (let ((V* (gensym 'v*))
	(V (gensym 'v))
	(LEN (gensym 'len)))
    `(let* ((,V* ,input*)
	    (,V (source-code ,V*))
	    (,LEN (improper-length ,V)))
       (if (negative? ,LEN)
	   (source-error ,V* "not a proper list")
	   ,(let rec ((clauses clauses*))
	      (cond ((null? clauses)
		     `(source-error ,V*
				    ,(let ((str (scm:objects->string
						 (map car (map cj-desourcify clauses*))
						 separator: " | ")))
				       (string-append
					"no match, expecting "
					(if (> (length clauses*) 1)
					    (string-append "any of " str)
					    str)))))
		    ((pair? clauses)
		     (let* ((clause* (car clauses))
			    (clause (source-code clause*))
			    (pro* (car clause))
			    (body (cdr clause))	;;(XX require body to be non-null?)
			    (pro (source-code pro*))
			    (len (improper-length pro))
			    )
		       (if (not (negative? len))
			   `(if (= ,LEN ,len)
				;; XX just *assume* that clause contents are just symbols for now
				,(let rec2 ((pro pro))
				   (cond ((null? pro)
					  (cons 'begin body))
					 ((pair? pro)
					  `(let* ((,(car pro) (car ,V))
						  ;; reuse var name (shadowing):
						  (,V (cdr ,V)))
					     ,(rec2 (cdr pro))))))
				,(rec (cdr clauses)))
			   ;; otherwise var arg match:
			   ;; (could optimize: if len is -1, the clause matches everything, no test needed and stop recursing, ignore remaining clauses (XXX croak about it when there *are* further clauses))
			   `(if (>= ,LEN ,(dec (- len)))
				;; XX dito above
				,(let rec2 ((pro pro))
				   (cond ((pair? pro)
					  `(let* ((,(car pro) (car ,V))
						  ;; reuse var name (shadowing):
						  (,V (cdr ,V)))
					     ,(rec2 (cdr pro))))
					 (else
					  `(let ((,pro ,V))
					     ,@body))))
				,(rec (cdr clauses))))))
		    (else
		     ;; can't use source-error here yet (because it has not been defined in this phase)
		     (error "invalid match syntax: expecting list of clauses, got:" clauses))))))))

;; require input to be a proper list (complain otherwise):
;; currently just an alias for match*, but I might change match* some time.
(define-macro* (match-list* input* . clauses)
  `(match* ,input* ,@clauses))


;; TESTs see simple-match.scm

(both-times
 (define (assert*-expand desourcify
			 gen-full-desourcify/1
			 pred
			 val
			 yes-cont
			 no-cont)
   (define V* (gensym 'v*))
   (define V (gensym 'v))
   `(let* ((,V* ,val)
	   (,V (,desourcify ,V*)))
      (if (,pred ,V)
	  ,(if yes-cont `(,yes-cont ,V) `(void))
	  ,(if (source-code no-cont)
	       no-cont
	       `(source-error ,V*
			      ,(string-append "does not match "
					      (scm:object->string
					       (cj-desourcify pred))
					      " predicate")
			      ,(gen-full-desourcify/1 V* V)))))))

(define-macro* (assert-desourcified* pred val #!optional yes-cont no-cont)
  (assert*-expand 'cj-desourcify
		  (lambda (V* V)
		    V)
		  pred val yes-cont no-cont))

;; only remove location information 1 level (uh, better names?)
(define-macro* (assert* pred val #!optional yes-cont no-cont)
  (assert*-expand 'source-code
		  (lambda (V* V)
		    `(cj-desourcify ,V*))
		  pred val yes-cont no-cont))

;; different from assert-desourcified* in two ways (1) pass the unwrapped result in
;; 'the same variable as' v instead of expecting a function, (2) evals
;; the input first.
(define-macro* (assert** pred var #!optional yes-expr no-expr)
  (assert-desourcified* symbol? var
	   (lambda (_)
	    `(assert-desourcified* ,pred (eval ,var)
		      (lambda (,var)
			,yes-expr)
		      ,@(if (source-code no-expr)
			    (list no-expr)
			    (list))))))


