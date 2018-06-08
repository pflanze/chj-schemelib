;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A matcher for static strings that implements the search via trie
;; like static code branching.

;; Wasteful for code size, for sure, especially with long match
;; keys. Only meant to be used with short keys and when it's used in a
;; hot path.

(require easy
	 trie)

(export (macro string-case)
	#!optional
	perhaps-let
	string-case-expand)

(include "cj-standarddeclares.scm")


;; in places where begin is not okay; what was the function for the
;; begin btw? XX Should all be in source-scheme.scm or something.
(def (perhaps-let es)
     (if (one-item? es)
	 (car es)
	 `(let () ,@es)))

(def (string-case-expand matchcases V notfound)
     (with-gensyms
      (LEN)
      `(let* ((,LEN (string-length ,V)))
	 ,(let rec ((t (alist.trie
			(map
			 (lambda (c)
			   (assert*
			    pair? c
			    (lambda (c*)
			      (let-pair
			       ((ms body) c*)
			       (assert*
				pair? ms
				(lambda (ms*)
				  (if (null? (cdr ms*))
				      (assert*
				       string? (first ms*)
				       (lambda (m)
					 (cons m (rest c*))))
				      (source-error
				       ms
				       "currently only supporting 1 match per branch"))))))))
			 matchcases)))
		    (i 0))
	    `(if (= ,i ,LEN)
		 ,(Maybe:if-let ((exprs (.Maybe-value t)))
				(perhaps-let exprs)
				notfound)
		 ,(let ((as (trie.entries-alist t)))
		    (if (null? as)
			notfound
			`(case (string-ref ,V ,i)
			   ,@(map (lambda-pair ((k t*))
					  `((,k) ,(rec t* (inc i))))
				  as)
			   (else ,notfound)))))))))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (string-case-expand '((("ab") 1) (("ac") 2) (("") 11)) 'ABC 'NOPE)
 (let* ((GEN:LEN-732 (string-length ABC)))
   (if (= 0 GEN:LEN-732)
       11
       (case (string-ref ABC 0)
	 ((#\a)
	  (if (= 1 GEN:LEN-732)
	      NOPE
	      (case (string-ref ABC 1)
		((#\b) (if (= 2 GEN:LEN-732) 1 NOPE))
		((#\c) (if (= 2 GEN:LEN-732) 2 NOPE))
		(else NOPE))))
	 (else NOPE)))))


;; The last case must (is this a good idea?) be an |else| case, all
;; the others must list strings to match for, the same way cases in
;; |case| list atoms. (Are strings atoms, too? "Immediates" is not it:
;; symbols are not immediates.)

(defmacro (string-case val . cases)
  (if (pair? cases)
      (let-pair ((lastcase cases*) (reverse cases))
		(mcase lastcase
		       (`(else . `else-exprs)
			(early-bind-expressions
			 ;; XX not hygienic btw, can conflict with our
			 ;; code, except if using ## / "" namespace
			 ;; everywhere, which we should...
			 (val)
			 ;; XX now also want a late-bind-expressions ?
			 (with-gensyms
			  (ELSE)
			  `(let ((,ELSE (lambda () ,@else-exprs)))
			     ,(string-case-expand cases* val `(,ELSE))))))))
      (source-error stx "need at least an else case")))

(TEST
 > (def (t v)
	(string-case v
		     (("ho") 'ho)
		     (("hi") 'yes)
		     (else 'nomatch)))
 > (t "ho")
 ho
 > (t "hi")
 yes
 > (t "hii")
 nomatch
 > (t "h")
 nomatch
 > (t "")
 nomatch
 > (t "hO")
 nomatch)

