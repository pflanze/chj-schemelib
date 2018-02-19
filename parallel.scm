;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (posix/interrupts interrupt-install-handler!
			   SIGCHLD)
	 )

(export (macros parallel-for..<*
		noparallel-for..<*
		parallel:for-all
		parallel-for-all
		noparallel-for-all)
	current-num-cpus-to-use)


(include "cj-standarddeclares.scm")

;; needs to be a '*' for, i.e. |to| needs to be known in advance so
;; that it can divide work (supposedly equally) in advance.

(define current-num-cpus-to-use (make-parameter #f))

(define-typed (_parallel-for..<*
	       #(natural? min-iter-per-child)
	       #(fixnum? from)
	       #(fixnum? to)
	       #(procedure? loopbody))
  (assert (< from to))
  (let* ((numiter (- to from))
	 (iterpercpu (/ numiter (current-num-cpus-to-use))))
    (if (>= iterpercpu min-iter-per-child)
	(begin
	  (interrupt-install-handler! SIGCHLD false/0)
	  ;;XXX ^ breaks subsequent compilation runs, for example. But
	  ;;(interrupt-remove-handler! SIGCHLD) doesn't properly undo
	  ;;it.
	  
	  ;;(##gc) ;; ok? speed? XXX  issue is, seems we need to stop interrupts? but anyway would be better for forking? wll does it matter?
	  (let* ((i+pids (map (lambda (i)
				(let ((lo (integer (* i iterpercpu)))
				      (hi (integer (* (inc i) iterpercpu))))
				  (cons i
					(posix:fork*
					 (thunk
					  (loopbody lo hi)
					  0)))))
			      (iota (current-num-cpus-to-use))))
		 (stati (map (lambda-pair
			      ((i pid))
			      (let ((status (s32vector 0)))
				(posix:waitpid pid status 0)
				(s32vector-ref status 0)))
			     i+pids)))
	    (assert (every zero? stati))
	    (void)))
	;; too small, don't fork anything
	;; XX: still fork a smaller number of children?
	(loopbody from to))))


(define-macro* (parallel-for..<* min-iter-per-child var-from-to . body)
  (mcase var-from-to
	 (`(`var `from `to)
	  (with-gensyms
	   (LP FROM TO)
	   `(_parallel-for..<* ,min-iter-per-child
			       ,from
			       ,to
			       (lambda (,FROM ,TO)
				 (let ,LP ((,var ,FROM))
				      (if (##fixnum.< ,var ,TO)
					  (begin
					    ,@body
					    (,LP (##fixnum.+ ,var 1)))))))))))

(define-macro* (noparallel-for..<* min-iter-per-child var-from-to . body)
  `(for..< ,var-from-to ,@body))


(define-macro* (parallel:for-all m ss is . body)
  (mcase ss
	 (`(`S0 `S1)
	  (mcase is
		 (`(`I0 `I1)
		  (with-gensyms
		   (M)
		   `(let ((,M ,m))
		      (letv ((,S0 ,S1) (.sizes ,M))
			    (for..<*
			     (,I0 0 ,S0)
			     (for..<*
			      (,I1 0 ,S1)
			      ,@body))))))))))
;; ? (parallel:for-all (iota 10 3) (x0 x1) (y1 y2) body)  XX docs and tests


(define-macro* (parallel-for-all min-iter-per-child m ss is . body)
  (mcase ss
	 (`(`S0 `S1)
	  (mcase is
		 (`(`I0 `I1)
		  (with-gensyms
		   (M)
		   `(let ((,M ,m))
		      (letv ((,S0 ,S1) (.sizes ,M))
			    (parallel-for..<*
			     ,min-iter-per-child
			     (,I0 0 ,S0)
			     (for..<*
			      (,I1 0 ,S1)
			      ,@body))))))))))

(define-macro* (noparallel-for-all min-iter-per-child m ss is . body)
  `(parallel:for-all ,m ,ss ,is ,@body))

