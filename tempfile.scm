;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 atomic-box
	 test
	 (stream stream-map)
	 (realrandom make-realrandom-string-stream)
	 (cj-io-util port.name
		     eexist-exception?
		     eperm-exception?
		     xxsystem))

(export tempfile)



(def random-appendices
     (delay
       (atomic-box
	(stream-map (C string.replace-substrings _ "/" "_")
		    (make-realrandom-string-stream #f)))))

(def (get-long-random-appendix)
     (atomic-box.update! (force random-appendices)
			 (lambda (s)
			   (let-pair ((str r) (force s))
				     (values r str)))))

(def (get-short-random-appendix)
     (substring (get-long-random-appendix) 0 10))


(def (randomly-retrying base get-random-appendix create)
     ;; long? is a hack to work around actually non-working
     ;; EEXCL. Well, could save all the exception catching in those
     ;; cases. lol.
     (let next ((tries 10))
       ;; h the only use of retries is to make the random part in the
       ;; path shorter
       (let ((path (string-append base (get-random-appendix))))
	 (with-exception-catcher
	  (lambda (e)
	    (if (and (eexist-exception? e)
		     (positive? tries))
		(next (dec tries))
		(raise e)))
	  (& (create path))))))


;; create-public-tmp-directory ?
;; can't make it private without using posix/ modules.
(def (public-tempdir #!key
		     #((maybe string?) perms)
		     #((maybe string?) group)
		     (base "/tmp/public-tempdir"))
     (randomly-retrying base
			get-short-random-appendix
			(lambda (path)
			  (create-directory path)
			  (if perms
			      (xxsystem "chmod" perms "--" path))
			  (if group
			      (xxsystem "chgrp" group "--" path))
			  path)))

(def tempfile-base
     ;; (string-append (getenv "HOME") "/.public-tempdir")
     (public-tempdir perms: "0700"))

;; this is only safe against overwriting of existing files thanks to
;; proper long random numbers
(def (tempfile #!optional (base (string-append tempfile-base "/")))
     -> string?
     (randomly-retrying base
			get-long-random-appendix ;; hack
			(lambda (path)
			  (close-port (open-output-file path))
			  path)))


;; to get the path, use port.name on the result
(def (open-tempfile #!optional (base (string-append tempfile-base "/")))
     -> output-port?
     (randomly-retrying base
			get-long-random-appendix ;; hack
			(lambda (path)
			  (open-output-file path))))

(def (call-with-tempfile proc/1 . args) -> string?
     ;; hmm, no 'dynamic wind' thing at all??
     (let ((p (apply open-tempfile args)))
       (proc/1 p)
       (close-port p)
       (port.name p)))


;; saves as utf-8
(def. (string.tempfile-path v) -> string?
  (call-with-tempfile (lambda (port)
			(display v port))))

(def. (u8vector.tempfile-path v) -> string?
  (call-with-tempfile (lambda (port)
			(write-u8vector v port))))



;; this is safe against overwriting, but it's got bad scaling
;; behaviour
(def (tempfile-incremental-at base #!optional (suffix "") (z 0))
     (lambda ()
       (let lp ()
	 (let ((path
		(with-exception-catcher
		 (lambda (e)
		   (if (eexist-exception? e)
		       (begin
			 (lp))
		       (raise e)))
		 (& (let ((path (string-append base
					       (number.string z)
					       suffix)))
		      (inc! z)
		      ;; only way to exclusively create a file on Gambit?
		      (create-symbolic-link "a" path)
		      path)))))
	   (let ((tmppath (tempfile base)))
	     (rename-file tmppath path)
	     path)))))

