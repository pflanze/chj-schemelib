
;; A "hashtable" implementation that uses the filesystem as storage,
;; but also (currently) doesn't have a .list or .keys method (only
;; .values).

(require easy
	 test
	 (skein skein:digest)
	 jclass
	 (cj-path path-string?)
	 tempfile)


(jclass (fstable #(path-string? basedir))

	(def-method (set! t #(string? key) #(string? val))
	  (let ((tf (tempfile (string-append (.basedir t) ".tmp")))
		(keyhash (skein:digest key)))
	    (call-with-output-file tf
	      (lambda (p)
		(display val p)))
	    (rename-file tf (string-append (.basedir t) "/" keyhash))))

	(def-method (ref t #(string? key) alternative)
	  (let* ((keyhash (skein:digest key))
		 (path (string-append (.basedir t) "/" keyhash)))
	    (with-exception-catcher
	     (lambda (e)
	       (if (no-such-file-or-directory-exception? e)
		   alternative
		   (raise e)))
	     (& (call-with-input-file path (C read-line _ #f))))))

	(def-method (delete! t #(string? key))
	  (let ((keyhash (skein:digest key)))
	    (delete-file (string-append (.basedir t) "/" keyhash)))))


(TEST
 > (def fstable:test-path ".fstable:test-dir")
 > (if (not (file-exists? fstable:test-path)) (create-directory fstable:test-path))
 > (def t (fstable fstable:test-path))
 > (.set! t "a" "b")
 > (.ref t "a" 'nah)
 "b"
 > (.ref t "x" 'nah)
 nah
 > (.delete! t "a")
 > (.ref t "a" 'nah)
 nah
 > (delete-directory fstable:test-path))

