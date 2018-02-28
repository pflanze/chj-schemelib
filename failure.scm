;;; Copyright 2015-2018 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; structured failure values, including |fail| wrapper around Failure

;; |fail| is basically error without throwing it (plus a 'kind' field,
;; and its message does not need the end ":").

;; (Can't use |error| for naming, even though this is inconsistent
;; with the naming of Error in Result.scm now. Sigh.)


(require easy
	 more-oo
	 (Result Error)
	 test)

(export failure
	failure.kind
	failure.message
	failure.arguments
	failure.string
	failure-of-kind
	fail)


(jclass (failure [symbol? kind]
		 [string? message]
		 [list? arguments])

	(def-method (string v)
	  (string-append
	   (symbol->string kind)
	   ": "
	   (if (null? arguments)
	       message
	       (string-append
		message
		(strings-join
		 (cons ":"
		       (map object->string arguments))
		 " "))))))


(def (failure-of-kind kind)
     (lambda (v)
       (and (failure? v)
	    (eq? (failure.kind v) kind))))


(TEST
 > (.string (failure 'foo "Heyey" '(10.3 1)))
 "foo: Heyey: 10.3 1"
 > (.string (failure 'foo "Heyey" '()))
 "foo: Heyey"
 > (.string (failure 'x "Heyey" '("A" (a "1"))))
 "x: Heyey: \"A\" (a \"1\")"
 > (def foo-failure? (failure-of-kind 'foo))
 > (foo-failure? (failure 'foo "bla" '()))
 #t
 > (foo-failure? 'foo)
 #f
 > (foo-failure? (failure 'bar "bla" '()))
 #f)


(def (fail #(symbol? kind) #(string? msg) . args)
     (Error (failure kind msg args)))

