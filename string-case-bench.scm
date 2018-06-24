;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 string-case
	 memcmp
	 random)

(include "cj-standarddeclares.scm")

(namespace ("string-case-bench#" t1 t2 t3))


(both-times
 (def string-case-bench:cases
      '("ho"
	"hi"
	"case"
	"cond"
	"let"
	"if"
	"string-case"
	"string-cond"
	"string="
	""
	"for"
	"foreach"
	"forall"
	"forever"
	"loop"
	"while"
	"not"
	"int"
	"hello world"
	"hello lovely world how are you today? it's been a long way."))

 (def string-case-bench:safe #t))


(def (t1 v)
     (IF (not string-case-bench:safe)
	 (declare (fixnum) (not safe)))
     (enable-unquoting
      (string-case v
		   ,@(map (lambda (str)
			    `((,str) ',(.symbol str)))
			  string-case-bench:cases)
		   (else 'nomatch))))

(def (t2 v)
     (IF (not string-case-bench:safe)
	 (declare (fixnum) (not safe)))
     (enable-unquoting
      (cond ,@(map (lambda (str)
		     `((string=? v ,str) ',(.symbol str)))
		   string-case-bench:cases)
	    (else 'nomatch))))


(use-memcmp)

(def (t3 v)
     (IF string-case-bench:safe
	 (assert (string? v)))
     (enable-unquoting
      (cond ,@(map (lambda (str)
		     `((memcmp:@string=? v ,str) ',(.symbol str)))
		   string-case-bench:cases)
	    (else 'nomatch))))


(def (string-case-bench str n)
     (assert (equal?* (time (repeat n (t1 str)))
		      (time (repeat n (t2 str)))
		      (time (repeat n (t3 str))))))


(def (string-case-bench2 str n)
     (assert (equal?* (time (repeat n (t1 str)))
		      (time (repeat n (t3 str))))))

(TEST
 > (for-each (lambda (str)
	       (assert (equal?* (string-case-bench#t1 str)
				(string-case-bench#t2 str)
				(string-case-bench#t3 str))))
	     (cons "nonexisting case" string-case-bench:cases)))

(def (string-case-bench-all n t)
     (time (for-each (lambda (str)
		       (repeat n (t str)))
		     string-case-bench:cases)))

(def (string-case-bench-nonmatches n t)
     (time (repeat 30
		   (let ((str (random-string (random-integer 40))))
		     (repeat n (t str))))))

