(require cj-env
	 posix/cj-posix
	 string-util ;; which one? grr, so..:
	 string-util-1
	 string-util-2
	 string-util-3
	 interrupts
	 (test TEST)
	 )

;; cj Thu, 21 Dec 2006 01:41:15 +0100

(define BUFSIZ 10)

(define (u8vector->string vec from to)
  ;; (of course that's inefficient)
  (list->string
   (map integer->char (u8vector->list (subu8vector vec from to)))))

(define (tr)
  ;; install a noop handler (to override the builtin one which clobbers the process statii):
  (interrupt-install-handler! SIGCHLD (& #f))
  (let* ((p (pipe))
	 (pid (fork*
	       (lambda ()
		 (close (filedescriptors-read p))
		 (dup2 (filedescriptors-write p) 1)
		 (exec ;;"bash" "-"
		  "perl" "-MTime::HiRes=sleep" "-we" "$|++; for(1..13){sleep 0.01; print \"abc\"; }")))))
    (close (filedescriptors-write p))
    (let ((p (fd->port (filedescriptors-read p) 'input))
	  (buf (make-u8vector BUFSIZ)))
      (let lp ()
	(let ((num (read-subu8vector buf 0 BUFSIZ p)))
	  (if (eq? num 0)
	      (waitpid* pid)
	      (let ((str (u8vector->string buf 0 num)))
		(string-tr! str "a" "Q")
		(println str)
		(lp))))))))

(TEST
 > (with-output-to-string "" tr)
 "QbcQbcQbcQ\nbcQbcQbcQb\ncQbcQbcQbc\nQbcQbcQbc\n"
 ;; (Note that read-subu8vector does not return upon the first write,
 ;; it waits until the buffer is exactly full (or till close). This is
 ;; as expected (blocking read semantics).)
 )

;; A simple example:
;; > (waitpid* (with-output-to-file "_HALLO" (& (fork* (& (println "Hallo Welt") (force-output (current-output-port)) 0)))))
;; The force-output is necessary since exit exits "violently".
