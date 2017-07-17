;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Compress an u8vector via a pipe to compression utils.

(require easy
	 jclass
	 posix/cj-posix
	 (cj-path path-string?)
	 (posix/interrupts interrupt-install-handler!
			   SIGCHLD)
	 test)


(export (jclass gzip-compressor)
	(jclass xz-compressor)
	(jclass lzop-compressor)
	(jclass bzip2-compressor)
	(jclass data-compressor))



;;XX move to lib?
(def (posix:pipe-values)
     (let ((v (posix:pipe)))
       (values (s32vector-ref v 0)
	       (s32vector-ref v 1))))


;; Compression levels:
;; 1= fastest, 9= best compression (default for gzip and xz is 6, for
;; lzop 3; xz starts natively at 0)

(def (1..9? v)
     (and (real? v)
	  (<= 1 v 9)))

(def (0..9? v)
     (and (real? v)
	  (<= 0 v 9)))



(jclass _data-compressor

	(def-method (compress-u8vector s #(u8vector? vec) #!optional len)
	  (let* ((len* (u8vector.length vec))
		 (len (if len
			  (-> len (lambda (v)
				    (and (exact-natural0? v)
					 (<= v len*))))
			  len*)))

	    (let-values (((compr-r compr-w) (posix:pipe-values))
			 ((info-r info-w) (posix:pipe-values)))
	      (let ((worker-pid

		     (posix:fork*
		      (&
		       (posix:close compr-r)
		       (posix:close info-r)
		       (interrupt-install-handler! SIGCHLD false/0)
		       ;; ^ remember, breaks e.g. compile-file,
		       ;; hence use in child only.
		       (letv ((orig-r orig-w) (posix:pipe-values))
			     (let ((cmd-pid

				    (posix:fork*
				     (&
				      (posix:close info-w)
				      (posix:dup2 orig-r 0)
				      (posix:close orig-r)
				      (posix:close orig-w)
				      (posix:dup2 compr-w 1)
				      (posix:close compr-w)
				      (apply posix:exec
					     (.command s)
					     (.arguments s))))))
				   
			       (let ((wrote-len (posix:write-u8vector orig-w vec len)))
				 (assert (= wrote-len len)))
			       (posix:close orig-w)
			       (let ((status+pid (posix:waitpid** cmd-pid)))
				 ;; XX doing so many complications how
				 ;; is this gonna be more efficient?
				 (assert (= 4 (posix:write-u8vector info-w (##u8vector-copy status+pid) 4)))
				 (posix:close info-w))
			       0))))))

		(posix:close compr-w)
		(posix:close info-w)

		(let* ((compr-port (fd->port compr-r 'RDONLY))
		       ;; be sure buffer is big enough for doing one call:
		       (compr-buflen (+ 30 (arithmetic-shift (* 3 len) -1)))
		       (compr (##make-u8vector compr-buflen))
		       (info-port (fd->port info-r 'RDONLY))
		       (info (make-u8vector 4)))
		  ;; don't use posix: calls here to not block.
		  ;; XXX does that now restart syscalls or not?
		  (let ((got-compr (read-subu8vector compr 0 compr-buflen compr-port)))
		    (close-port compr-port)
		    (let ((got-info (read-subu8vector info 0 4 info-port)))
		      (close-port info-port)
		      (if (and (number? got-info)
			       (= 4 got-info))
			  (let ((status (##s32vector-ref info 0))) ;; hacky?
			    (if (zero? status)
				(begin
				  (u8vector-shrink! compr got-compr)
				  compr)
				(error "compress-u8vector: compressor exited with status:" status)))
			  (error "compress-u8vector: unknown failure of child, got (instead of 4 bytes):"
				 got-info)))))))))



	(jclass ((data-compressor/level _data-compressor/level)
		 #!key
		 #((maybe (list-of string?)) options))

		(def-method* (arguments s)
		  (let ((opts (or options
				  (.default-options s)))
			(level (.level s)))
		    (if level
			(append opts
				(list (string-append "-" (number.string (integer level)))))
			opts)))

		(jclass ((data-compressor/level-1..9 _data-compressor/level-1..9)
			 #((maybe 1..9?) level))

			(jclass (gzip-compressor)

				(def-method (command s)
				  "gzip")
				(def-method (default-options s)
				  '("--no-name")))
	
			(jclass (lzop-compressor)

				(def-method (command s)
				  "lzop")
				(def-method (default-options s)
				  '("--no-name")))

			(jclass (bzip2-compressor)

				(def-method (command s)
				  "bzip2")
				(def-method (default-options s)
				  ;; no "--no-name" option?
				  '())))

		(jclass ((data-compressor/level-0..9 _data-compressor/level-0..9)
			 #((maybe 0..9?) level))

			(jclass (xz-compressor)

				(def-method (command s)
				  "xz")
				(def-method (default-options s)
				  ;; no "--no-name" option, how to
				  ;; avoid embedding a timestamp?
				  '()))))

	(jclass (data-compressor #(path-string? command)
				 #!optional
				 (#((list-of string?) arguments) '()))))



(TEST
 > (.compress-u8vector (data-compressor "true" (list)) (u8vector 1 2 3 4))
 #u8()
 > (%try-error (.compress-u8vector (data-compressor "false") (u8vector 1 2 3 4)))
 #(error "compress-u8vector: compressor exited with status:" 256)
 > (.compress-u8vector (gzip-compressor) (.u8vector "Hello"))
 #u8(31 139 8 0 0 0 0 0 0 3 243 72 205 201 201 7 0 130 137 209 247 5 0 0 0)
 > (.compress-u8vector (gzip-compressor level: 9) (.u8vector "Hello"))
 #u8(31 139 8 0 0 0 0 0 2 3 243 72 205 201 201 7 0 130 137 209 247 5 0 0 0)
 > (with-output-to-file ".data-compressor.test.gz" (& (write-subu8vector # 0 (.length #))))
 > (read-line (open-process (list path: "zcat"
				  arguments: '(".data-compressor.test.gz"))) #f)
 "Hello"
 > (delete-file ".data-compressor.test.gz")

 > (%try-error (gzip-compressor level: 0))
 #(error "level does not match (maybe 1..9?):" 0)
 > (.length (.compress-u8vector (xz-compressor level: 0) (.u8vector "Hello")))
 ;; if this test fails please just comment out the value:
 37)

