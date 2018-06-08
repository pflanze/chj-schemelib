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

;; Obvious potential optimizations:

;; - check string length for possible matches before going down
;;   further? (only reasonably possible when there's only one string
;;   left?)

;; - or rather right away: in the 'last mile' use memcmp for the whole
;;   of the remainder (and also something similar for the intermediate
;;   stretches?)

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
	 (declare (fixnum) (not safe))
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


;; The last case must (is this a good idea?) be an |else| case, all
;; the others must list strings to match for, the same way cases in
;; |case| list atoms. (Are strings atoms, too? "Immediates" is not it:
;; symbols are not immediates.)

(defmacro (string-case val . cases)
  (if (pair? cases)
      (let-pair ((lastcase cases*) (reverse cases))
		(mcase lastcase
		       (`(else . `else-exprs)
			;; Do *not* use early-bind-expressions here,
			;; because it would break safety in the face
			;; of threads. (Also, don't have a
			;; late-bind-expressions, so have to code that
			;; part manually, too :)
			(with-gensyms
			 (V ELSE)
			 `(let ((,V ,val)
				(,ELSE (lambda () ,@else-exprs)))
			    ,(string-case-expand cases* V `(,ELSE)))))))
      (source-error stx "need at least an else case")))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#string-case ABC (("ab") 1) (("ac") 2) (("") 11) (else NOPE))
 (let ((GEN:V-1 ABC)
       (GEN:ELSE-766 (lambda () NOPE)))
   (let* ((GEN:LEN-767 (string-length GEN:V-1)))
     (declare (fixnum) (not safe))
     (if (= 0 GEN:LEN-767)
	 11
	 (case (string-ref GEN:V-1 0)
	   ((#\a)
	    (if (= 1 GEN:LEN-767)
		(GEN:ELSE-766)
		(case (string-ref GEN:V-1 1)
		  ((#\b) (if (= 2 GEN:LEN-767) 1 (GEN:ELSE-766)))
		  ((#\c) (if (= 2 GEN:LEN-767) 2 (GEN:ELSE-766)))
		  (else (GEN:ELSE-766)))))
	   (else (GEN:ELSE-766)))))))

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

