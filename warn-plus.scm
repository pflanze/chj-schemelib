;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (simple-match-1 warn*)
	 (cj-env-2 object->serial-number-string)
	 symboltable
	 test
	 (string-util-1 string-split)
	 (string-util-3 with-error-to-string .drop-while))

(export current-WARN-mode
	(macro variables)
	(macro WARN)
	(macro WARN-ONCE))


(defmacro (variables . vs)
  (list 'quasiquote
	(fold-right (lambda (v rest)
		      (cons (symbol->keyword (source-code v))
			    (cons (list 'unquote v)
				  rest)))
		    '()
		    vs)))

(TEST
 > (let ((a 1) (b "hey")) (variables a b))
 (a: 1 b: "hey"))


(defenum WARN-mode
  warn-only
  warn/continuation)

;; #f turning warnings off, or a WARN-mode
(defparameter current-WARN-mode warn/continuation)

(def (warn-plus:_WARN loc opt [string? message] args)
     (let ((cont (lambda (msg)
		   (apply location-warn loc msg args))))
       (if (eq? opt 'warn/continuation)
	   (continuation-capture
	    (lambda (c)
	      (cont (string-append
		     "#"
		     (object->serial-number-string c)
		     " -- "
		     message))))
	   (cont message))))

(defmacro (WARN message . args)
  (let ((loc (source-location stx)))
    (with-gensym
     OPT
     `(##cond ((current-WARN-mode)
	       => (lambda (,OPT)
		    (warn-plus:_WARN ',loc ,OPT ,message (##list ,@args))))))))


(TEST
 > (def (trim-digitdot-left str)
	(.drop-while str (either char-digit? (char-one-of?/ "."))))
 > (def (t proc)
	(=> (fst (with-error-to-string proc))
	    (string-split (char-one-of?/ "@#"))
	    rest
	    (.map trim-digitdot-left)))
 
 > (parameterize
    ((current-WARN-mode warn-only))
    (local-TEST
     > (%try (WARN '(a "ha")))
     (exception text: "message does not match string?: (a \"ha\")\n")
     > (t (& (WARN "" '(a "ha"))))
     ;; originally something like:
     ;;    "*** WARNING IN (console)@11.26 -- : (a \"ha\")\n"
     (" -- : (a \"ha\")\n")
     > (t (& (WARN "this" '(a "ha"))))
     (" -- this: (a \"ha\")\n")
     > (t (& (WARN "this")))
     (" -- this\n")))

 > (parameterize
    ((current-WARN-mode warn/continuation))
    (local-TEST
     > (t (& (WARN "" '(a "ha"))))
     ;; originally something like:
     ;; *** WARNING IN (console)@11.1 -- #2 -- : (a "ha")
     (" -- " " -- : (a \"ha\")\n")
     > (t (& (WARN "this" '(a "ha"))))
     (" -- " " -- this: (a \"ha\")\n")
     > (t (& (WARN "this")))
     (" -- " " -- this\n")))

 > (parameterize
    ((current-WARN-mode #f))
    (local-TEST
     > (%try (WARN '(a "ha")))
     (value #!void)
     > (t (& (WARN "" '(a "ha"))))
     ()
     > (t (& (WARN "this" '(a "ha"))))
     ()
     > (t (& (WARN "this")))
     ())))


;; Warnings that show up once (or some fixed times) only:

;; note: count will always just be 1 unless higher number of times is
;; required
(def warn-plus:warned-count (symboltable))

(def (warn-plus:warn?-inc! sym)
     (if (symboltable-ref warn-plus:warned-count sym #f)
	 #f
	 (begin
	   (set! warn-plus:warned-count
		 (symboltable-set warn-plus:warned-count sym 1))
	   #t)))

(defmacro (WARN-ONCE message . args)
  (let ((key (gensym 'warn-once-name)))
    `(if (warn-plus:warn?-inc! ',key)
	 (WARN ,message ,@args))))

