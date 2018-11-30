;;; Copyright 2006-2007 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; TODO: eliminate

(require cj-env ;; testing only
	 test
	 (test-lib-1 %try))

(export string-list->string
	#!optional
	@string-copy!_len
	@string-copy!_end
	my-string-append
	string-copy!)

;; (compile #t)


(define (string-list->string strings)
  (let* ((totlen (let lp ((l strings)
			  (len 0))
		   (if (null? l) len
		       (let ((a (car l)))
			 (if (string? a)
			     (lp (cdr l)
				 (+ len (string-length a)))
			     (error "not a string:" a))))))
	 (out (##make-string (if (fixnum? totlen)
				 totlen
				 (error "length overflow")))))
    (let lp ((l strings)
	     (i 0))
      (if (null? l) out
	  (let* ((str (car l))
		 (len (string-length str)))
	    (@string-copy!_len out i str 0 len)
	    (lp (cdr l)
		(+ i len)))))))


(c-declare "#include <string.h>")


;; IMPORTANT: arguments are not checked for correct type or whether
;; they are within correct bounds.


;; @string-copy! has been taking a len argument, unlike the srfi-13
;; function. So I've renamed it to @string-copy!_len now.

(define (@string-copy!_len target tstart s start len)
  (##c-code "
#define charwidth_in_bytes ___CS
memcpy(
       ((char*)___BODY(___ARG1)) + charwidth_in_bytes*___INT(___ARG2),
       ((char*)___BODY(___ARG3)) + charwidth_in_bytes*___INT(___ARG4),
       charwidth_in_bytes*___INT(___ARG5)
      );
___RESULT=___VOID;/*avoid warning*/
#undef charwidth_in_bytes
" target tstart s start len))


;; This is using the same interface as string-copy! from srfi-13. I'm
;; naming it @string-copy!_end to prevent problems with code using the
;; previous variant from above.

(define (@string-copy!_end target tstart s start end)
  (##c-code "
#define charwidth_in_bytes ___CS
memcpy(
       ((char*)___BODY(___ARG1)) + charwidth_in_bytes*___INT(___ARG2),
       ((char*)___BODY(___ARG3)) + charwidth_in_bytes*___INT(___ARG4),
       charwidth_in_bytes*(___INT(___ARG5)-___INT(___ARG4))
      );
___RESULT=___VOID;/*avoid warning*/
#undef charwidth_in_bytes
" target tstart s start end))


(define (unsigned-fixnum? v)
  (and (fixnum? v)
       (fx>= v 0)))

(define (string-copy! target tstart s start end)
  (if (string? target)
      (if (unsigned-fixnum? tstart)
	  (if (string? s)
	      (if (unsigned-fixnum? start)
		  (if (unsigned-fixnum? end)
		      (if (>= end start)
			  (if (<= (+ tstart (- end start)) (string-length target))
			      (if (<= end (string-length s))
				  (@string-copy!_end target tstart s start end)
				  (error "trying to read past end of s:" end))
			      (error "trying to write past end of target:" tstart start end))
			  (error "start not before end:" start end))
		      (error "not an unsigned fixnum:" end))
		  (error "not an unsigned fixnum:" start))
	      (error "not a string:" s))
	  (error "not an unsigned fixnum:" tstart))
      (error "not a string:" target)))

(TEST
 > (define ! (lambda (a b c d e)
	       (string-copy! a b c d e)
	       a))
 > (! "aha" 0 "b" 0 1)
 "bha"
 > (%try (! "" 0 "b" 0 1))
 (exception text: "trying to write past end of target: 0 0 1\n")
 > (! "aha" 0 "be" 1 2)
 "eha"
 > (%try (! "aha" 0 "be" 1 3))
 (exception text: "trying to read past end of s: 3\n")
 > (! "aha" 2 "be" 1 2)
 "ahe"
 > (! "aha" 2 "be" 0 1)
 "ahb"
 > (%try (! "aha" 2 "be" 0 2))
 (exception text: "trying to write past end of target: 2 0 2\n"))

;; NOTE: there are no tests for fixnum over/underflow, which is
;; [probably] only okay while using safe mode in this module.


; (define (cj-string-append . strings)
;   (string-list->string strings))


; (define (test-@string-copy! n s1 start1 s2 start2 len)
;   (*do-times n
; 	     (@string-copy! s1 start1 s2 start2 len)))

; (define (test-c-code n arg)
;   (*do-times n
; 	     (##c-code "___RESULT= ___ARG1;" arg)))

; (define (test-noop n arg)
;   (*do-times n
; 	     arg))

