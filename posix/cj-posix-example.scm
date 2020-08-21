;;; Copyright 2006-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
	 posix/cj-posix
	 string-util ;; which one? grr, so..:
	 string-util-1
	 string-util-2 ;; incl. string-find-char, string-tr
	 string-util-3
	 posix/interrupts
	 (test TEST)
         (values values->vector))


(define BUFSIZ 10)

(define (u8vector->string vec from to)
  ;; (of course that's inefficient)
  (list->string
   (map integer->char (u8vector->list (subu8vector vec from to)))))


(define (tr)
  ;; install a noop handler (to override the builtin one which
  ;; clobbers the process statii):
  (interrupt-install-handler! SIGCHLD (& #f))
  (let* ((p (posix:pipe))
	 (pid (posix:fork*
	       (lambda ()
		 (posix:close (filedescriptors-read p))
		 (posix:dup2 (filedescriptors-write p) 1)
		 (posix:exec ;;"bash" "-"
		  "perl"
                  "-MTime::HiRes=sleep"
                  "-we"
                  "$|++; for(1..13){sleep 0.01; print \"abc\"; }")))))
    (posix:close (filedescriptors-write p))
    (let ((p (fd->port (filedescriptors-read p) 'input))
	  (buf (make-u8vector BUFSIZ)))
      (let lp ()
	(let ((num (read-subu8vector buf 0 BUFSIZ p)))
	  (if (eq? num 0)
	      (posix:waitpid* pid)
	      (begin
		(println (string-tr (u8vector->string buf 0 num) "a" "Q"))
		(lp))))))))

(TEST
 > (fst (with-output-to-string tr))
 "QbcQbcQbcQ\nbcQbcQbcQb\ncQbcQbcQbc\nQbcQbcQbc\n"
 ;; (Note that read-subu8vector does not return upon the first write,
 ;; it waits until the buffer is exactly full (or till close). This is
 ;; as expected (blocking read semantics).)
 )

;; A simple example:
;; > (waitpid* (with-output-to-file "_HALLO" (& (fork* (& (println "Hallo Welt") (force-output (current-output-port)) 0)))))
;; The force-output is necessary since exit exits "violently".
