;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Newer, better, higher-level IO routines (than the cj-io-util ~mess)

(require easy
	 (cj-env-2 future)
	 (cj-io-util open-process*)
	 (cj-path path-string?)
	 (Status Result Failure)
	 test)

(export (class command)
	process-run
	string-writer
	string-reader

	#!optional
	process-spec?)


(def (process-spec? v)
     (or (null? v)
	 (and (pair? v)
	      (let-pair ((a v*) v)
			(and (keyword? a)
			     (pair? v*)
			     (let-pair ((b v**) v*)
				       (and (or (symbol? b)
						(string? b))
					    (process-spec? v**))))))))

(TEST
 > (process-spec? '())
 #t
 > (process-spec? '(a))
 #f
 > (process-spec? '(a:))
 #f
 > (process-spec? '(a: "a"))
 #t
 > (process-spec? '("a:" "a"))
 #f
 > (process-spec? '(a: "a" b: b))
 #t)


(class command
       (struct #(path-string? path)
	       #!key
	       (#(process-spec? additional-spec) '())
	       #!rest
	       #((list-of string?) arguments))

       (method (process-spec c . args)
	       `(path: ,(.path c)
		       arguments: ,(.arguments c)
		       ;; XX do the following two need merging?
		       ,@(.additional-spec c)
		       ,@args)))

(TEST
 > (command "a" additional-spec: '(foo: "a") "b")
 #((command) "a" (foo: "a") ("b"))
 > (command "a" "b")
 #((command) "a" () ("b")))


(def (process-run cmd reader writer ok?)
     (let* ((p (open-process* (.process-spec cmd
					     stdin-redirection: #t
					     stdout-redirection: #t)))
	    (f (future (writer p)))
	    (res (reader p)))
       (close-port p)
       (thread-join! f) ;; should exceptions be suppressed here?
       (let ((s (process-status p)))
	 (if (ok? s)
	     (Result res)
	     (Failure s)))))

(def (string-writer #(string? s))
     (lambda (p)
       (display s p)
       (close-output-port p)))

(def string-reader
     (lambda (p)
       (read-line p #f)))

(TEST
 > (process-run (command "tr" "a" "b")
		string-reader
		(string-writer "Hallo")
		zero?)
 #((Result) "Hbllo")
 > (process-run (command "tr" "a" "b")
		string-reader
		(string-writer "Hallo")
		(complement zero?))
 #((Failure) 0)
 ;; BTW status codes should have their own type, too, right. Then we
 ;; would see it perfectly well-informed here, too !
 )

