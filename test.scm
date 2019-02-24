;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Usage / workings:

;; - add (TEST ) forms containing copies of forms entered in the repl
;;   and their output, including the |>| symbol

;; - objects created through define-type cannot be used

;; - #345 etc. cannot be used currently; # and ## etc. *can* be used
;;   though

;; - (define ..) (as well as other side effects) can be used as well,
;;   just follow it immediately by another |>| (or the end of the TEST
;;   form).

;; - put (define TEST:equal? your-own-equal?) as a side-effecting form
;;   into the TEST form to redefine the equal check procedure that's
;;   being used.

;; - if the tests have to be run in a different namespace than the
;;   default one, put a (namespace: ("namespace#" identifiers)) form
;;   inside the TEST form before the part that needs it. Yes I
;;   consider this a kludge, but there's no way around it with the
;;   given infrastructure.

;; The test code is being stored as separate files, in the same
;; directory as the source files, with .test- prepended to the file
;; name. This is done so that the test cases don't occupy any space in
;; the program binary (and don't slow down compilation). The test
;; cases are run by calling |run-tests| with the source file paths (or
;; the paths without the suffix) for which you want to run tests, or
;; without argument if all tests should be run. The tests are run in
;; the interpreter. Test failures don't stop execution of |run-tests|,
;; they just output failure warnings (with source location
;; information). Passing verbose: #t to |run-tests| (this is actually
;; the default now) will give some information about which tests are
;; being run.

;; NOTE: to handle reloading of modules with TEST forms into the
;; running system, |load| is being redefined. There might still be
;; problems with that, though.

;; NOTE: TEST forms can't be used at compile-time (it will currently
;; give "Unbound variable: TEST#loaded"). Main problem is that their
;; contents would be written to the same external file as the rest,
;; and then at run time their dependencies might not be around.

;; "Repl" history is supported; it can be cleaned by running
;; (delete-repl-history!).

;; There's an ignore mechanism, documentation see "Ignore mechanism:"
;; below.


(require define-macro-star
	 cj-phasing
	 ;; cj-env-1 included in cj-source
	 (fixnum inc dec)
	 cj-source
	 simple-match-1
	 srfi-1
	 ;; (cj-io-util file-mtime) -- cycle
	 ;;cj-warn -- cycle
	 (list-util-1 map/tail)
	 (string-util-4 string-empty?))

(export (macro TEST)
	run-tests
	load ;; ? global override needed? XX

	#!optional
	TEST:parse/)

(include "cj-standarddeclares.scm") ;; -1--include.scm really?


;; copy from cj-functional to avoid circular dependency:
(define (complement fn)
  (lambda v
    (not (apply fn v))))

;; modified copy (no hooks usage, sadly) from cj-warn to avoid
;; circular dependency:
(define (test:warn msg . objs)
  (let ((port (current-error-port))
	(separator " "))
    (display msg port)
    (let lp ((objs objs))
      (cond ((null? objs)
	     (newline port))
	    ((pair? objs)
	     (display separator port)
	     (display (scm:object->string (car objs)) port)
	     (lp (cdr objs)))
	    (else (error "improper list:" objs))))))

;; string-util-2:
(define (string-ref* str i)
  (if (negative? i)
      (let ((len (string-length str)))
	(string-ref str (+ len i)))
      (string-ref str i)))
(define (chomp str)
  (if (%string-empty? str)
      str
      (if (char=? (string-ref* str -1) #\newline) ;; Perl even ignores \r heh
	  (substring str 0 (dec (string-length str)))
	  str)))

;; cj-io-util:
(define (read-lines #!optional (p (current-input-port)) (tail '()))
  (let rec ()
    (let ((line (read-line p)))
      (if (eof-object? line)
	  tail
	  (cons line (rec))))))

;; can't use string-starts-with? from string-util-2.scm
(define (test:string-starts-with? str substr)
  (let ((len0 (string-length str))
	(len1 (string-length substr)))
    (and (>= len0 len1)
	 (string=? (substring str 0 len1) substr))))

;; > (test:string-starts-with? "foo" "f")
;; #t
;; > (test:string-starts-with? "foo" "o")
;; #f
;; > (test:string-starts-with? "foo" "foo")
;; #t
;; > (test:string-starts-with? "foo" "foo1")
;; #f
;; > (test:string-starts-with? "foo1" "foo")
;; #t

;; string-util-3:
(define (substring* str i)
  (substring str i (string-length str)))


;;/copy


;; This is the *only* binding that has to be available to make loading
;; of code compiled with TESTs work.
;; Solve this?

(compile-time
 ;; make sure that TEST forms used at compile-time will fail even if
 ;; test.scm has been loaded previously:
 ;;(define TEST#loaded #!unbound)
 ;;but, breakes Compile/loader.scm, so:   XXX TODO find a solution?
 (define TEST#loaded #f))

(define TEST#loaded #f) (set! TEST#loaded '())


(both-times
 (define-macro (TEST . args) `(begin))
 ;; why do I have to (re?)add these? did I clean that up and now
 ;; readding I guess

 (define TEST:outports (make-table))
 ;; to suppress double outputs:
 (define TEST:seen (make-table))

 (define load:counter 0)
 (define load:mtime (make-table))
 ;; path (with .oX or .scm or nothing?) -> (pair-of float? natural?),
 ;; (cons mtime load:counter)

 ;; XX duplicate from cj-io-util for dependency reasons
 (define (file-mtime path)
   (time->seconds
    (file-info-last-modification-time
     (file-info path))))
 
 ;; adapted version from Gambit lib/_eval.scm
 (define (orig-load path-or-settings)
   ;;(macro-force-vars (path-or-settings)
   (##load path-or-settings
	   (lambda (script-line script-path) #f)
	   #t
	   #t
	   #f))
 ;; wrapped for us:
 (define (load path-or-settings)
   ;; XXX only works for string for now
   (define (fall-through msg)
     ;;sgh is warn loaded already?
     (println (list "load override from test.scm: "
		    msg))
     (orig-load path-or-settings))
   (if (string? path-or-settings)
       ;; now is it a source file? we hope so.. well check.
       ;; (if it's an object file, we won't know the source safely anyway)
       ;;sgh where is basename.
       (let ((path path-or-settings)
	     (load*
	      (lambda (sourcefile)
		;; record timestamp
		(table-set! load:mtime sourcefile
			    (cons (file-mtime sourcefile)
				  ;; don't have inc! yet
				  (let ((c load:counter))
				    (set! load:counter (+ c 1))
				    c)))
		
		(let ((outfile (TEST:sourcepath->testfilepath sourcefile)))
		  (cond ((table-ref TEST:outports outfile #f) => close-port))
		  (table-set! TEST:outports outfile)
		  ;; XXX oh, TEST:seen should have been by file,
		  ;; too. this assumes there is no cross file importnc
		  ;; (compiletimeload).
		  (set! TEST:seen (make-table))
		  ;; actually load the file
		  (orig-load path-or-settings)))))
	 (let ((dir (path-directory path))
	       (name (path-strip-directory path)))
	   ;; strip the suffix from name, if any
	   (name->basename+maybe-suffix
	    name
	    (lambda (basename maybe-suffix)
	      (cond ((and maybe-suffix
			  (string=? maybe-suffix "scm"))
		     (load* path))
		    (else
		     ;; XXX this assumes that object files are in the
		     ;; same directory as source files. warn?
		     (load* (string-append dir basename ".scm"))))))))
       (fall-through "only properly works for string arguments")))

 (define (name->basename+maybe-suffix name cont)
   (let lp ((n (reverse (string->list name)))
	    (suffix? '()))
     (if (null? n)
	 (cont name #f)
	 (let ((a (car n))
	       (r (cdr n)))
	   (if (char=? a #\.)
	       (cont (list->string (reverse r))
		     (list->string suffix?))
	       (lp r
		   (cons a suffix?)))))))
 ;; Note: the definition of TEST in this scope (above) prevents these
 ;; from being run:
 (TEST 
  > (name->basename+maybe-suffix "hello" cons) 
  ("hello" . #f)
  > (name->basename+maybe-suffix "hello.scm" cons) 
  ("hello" . "scm")
  > (name->basename+maybe-suffix "hel.lo.scm" cons) 
  ("hel.lo" . "scm")
  > (name->basename+maybe-suffix ".scm" cons) 
  ("" . "scm")
  > (name->basename+maybe-suffix "" cons) 
  ("" . #f)
  > (name->basename+maybe-suffix "/bar.x/foo" cons)
  ("/bar" . "x/foo") ;; nope, that was perhaps bad,
  ;; ("/bar.x/foo" . #f) or let it be. name. not path.
  )

 (define (path-maybe-suffix path)
   (name->basename+maybe-suffix (path-strip-directory path)
				(lambda (a b)
				  b)))

 ;; (define (path-ends-in-.scm? str)
 ;;   (cond ((path-maybe-suffix str)
 ;; 	  => (lambda (suff)
 ;; 	       (string=? suff "scm")))
 ;; 	 (else #f)))
 
 (define (perhaps-add-.scm str)
   ;; in fact if-not-already has .scm
   (cond ((path-maybe-suffix str)
	  => (lambda (suff)
	       (if (string=? suff "scm")
		   ;; already has .scm
		   str
		   ;; has some other suffix; hm, shouldn't happen but
		   ;; pass along, too
		   str)))
	 (else
	  (string-append str ".scm"))))

 
 (define (TEST:parse/ fold-sideeffect fold-test)
   (lambda (forms stx)
     (let rec ((forms forms)
	       (maybe-namespace-form #f))
       (define (rec/rest rest) 
	 (rec rest
	      maybe-namespace-form))
       (match-list*
	forms
	(() '())
	((form . rest)
	 (let ((form* (source-code form)))
	   (cond
	    ((eq? form* '>)
	     (match-list*
	      rest
	      (()
	       (source-error stx "expecting form after |>| at end of TEST form"))
	      ((expr . rest)
	       (let ((understand-as-sideeffect
		      (lambda ()
			(fold-sideeffect expr (rec/rest rest)))))
		 (match-list*
		  rest
		  (() (understand-as-sideeffect))
		  ((next . rest)
		   ;; btw O(n^2) because of rest scanning for proper
		   ;; list right. should have match* for that right?.
		   (let ((understand-as-test
			  (lambda ()
			    (fold-test expr next maybe-namespace-form
				       (rec/rest rest)))))
		     (if (eq? (source-code next) '>)
			 ;; could be either the result value for the
			 ;; test, or the start of the next test case;
			 ;; look ahead further
			 (match-list*
			  rest
			  (() (understand-as-test))
			  ((next2 . rest2)
			   (if (eq? (source-code next2) '>)
			       (understand-as-test)
			       (understand-as-sideeffect))))
			 (understand-as-test)))))))))
	    ((and (pair? form*)
		  (eq? (source-code (car form*))
		       'namespace:))
	     (rec rest
		  `(##namespace ,@(cdr form*))))
	    (else
	     (source-error form "expect |>|, got" (cj-desourcify form))))))))))

 (define TEST:conv
   (TEST:parse/
    ;; fold-sideeffect
    (lambda (expr rest)
      (cons
       ;; want to still allow # functionality. (Relying on
       ;; ##repl-result-history-ref being the same (and safe) as
       ;; repl-result-history-ref. Sick, hack over hack just because
       ;; we don't have proper macros?)
       `(##begin
	 (##define repl-result-history-ref TEST:repl-result-history-ref)
	 (test:current-test-location ',(source-location expr))
	 ,expr
	 (##define repl-result-history-ref ##repl-result-history-ref))
       rest))
    ;; fold-test
    (lambda (expr result maybe-namespace-form rest)
      (cons (cj-sourcify-deep
	     `(TEST:check
	       (begin
		 (test:current-test-location
		  ',(source-location expr))
		 (let ((repl-result-history-ref
			TEST:repl-result-history-ref))
		   ,@(if maybe-namespace-form
			 (list maybe-namespace-form)
			 (list))
		   ,expr))
	       ',result
	       ',(source-location expr)
	       TEST:equal?)
	     expr)
	    rest)))))


;; Ignore mechanism:

;; Plain text files, by default at .test-ignore.txt, contain one
;; to-be-ignored warning or error message by line (well, the first
;; line of the message is looked up only, so multi-line messages won't
;; hurt; the first line contains the location, which is currently
;; deemed the only reasonable anchor).

;; Examples:

;; *** WARNING IN "lib/list-util.scm"@515.4 -- TEST failure, got: (a b)
;; ERROR "lib/typed-list.scm"@277

;; The WARNING lines can just be copied verbatim (and optionally the
;; path changed into a relative one). For errors, the location of the
;; test form which leads to the error has to be retrieved, by walking
;; the stack. There's no need to give the column for error locations
;; (it is ignored if given). Note that when ignoring an error, the
;; remainder of the TEST form in which it occurs is skipped!


;; Table of "*** WARNING IN ..." strings (first line only) to ignore
;; warnings, and "ERROR ..locationinfo.." strings to ignore
;; errors. Both cases have file locations normalized (well, just
;; path-expand'ed).
(define current-test-ignores (make-parameter (make-table)))
;; ^ can't use define-if-not-defined easily since its definition
;; (cj-env) has some dependencies, also tests

(define (test:ignore-warning? str)
  (table-ref (current-test-ignores)
	     (string-first-line str)
	     #f))


(define (test:path-normalize-for-lookup p base)
  ;; had path-expand, for some reason? But, fails when re-using lib
  ;; via symlink from multiple places, as different absolute paths
  ;; (*not* normalized) will be in the locations. But it seems we
  ;; normalize somewhere else so, compare normalized with normalized?
  ;; Seems to fix the issue right now but, XX ugh.
  (path-normalize p base))

(define (test:error-location-string l base)
  (string-append "ERROR "
		 (location-string
		  l
		  normalize: (lambda (p)
			       (test:path-normalize-for-lookup p base))
		  omit-column?: #t)))

(define (test:warning-location-string l rest base)
  (string-append "*** WARNING IN "
		 (location-string
		  l
		  normalize: (lambda (p)
			       (test:path-normalize-for-lookup p base)))
		 " -- " rest))

(define (test:ignore-error? l base)
  (table-ref (current-test-ignores)
	     (test:error-location-string l base)
	     #f))


(define (test:read-ignore-parse line)
  ;; XX move this parsing stuff to an early lib, really? !
  (let* ((err (lambda (msg)
		;; XX show location
		(error (string-append "invalid format of ignore file line ("
				      msg "):")
		       line)))

	 (_-read-string-expect
	  (lambda (allow-eof?)
	    (lambda (p str)
	      (let ((len (string-length str)))
		(let lp ((i 0))
		  (if (< i len)
		      (let ((c (read-char p)))
			(if (eof-object? c)
			    (if allow-eof?
				#f
				(err (string-append
				      "premature end of string, expecting: "
				      str)))
			    (let ((c-expected (string-ref str i)))
			      (if (char=? c c-expected)
				  (lp (inc i))
				  (err (string-append
					"expecting "
					(object->string c-expected)
					", got"
					(object->string c)))))))
		      (void)))))))

	 (read-string-expect
	  (_-read-string-expect #f))

	 (maybe-read-string-expect
	  (_-read-string-expect #t))

	 (read-until
	  (lambda (p pred)
	    (let lp ((cs '()))
	      (let ((c (read-char p)))
		(if (pred c)
		    (list->string (reverse cs))
		    (lp (cons c cs)))))))

	 (ra (lambda (tag str)
	       (call-with-input-string
		str
		(lambda (p)
		  (let* ((loc-file (read p))
			 (@ (read-string-expect p "@"))
			 (loc-ended? #f)
			 (loc-line
			  (string->number
			   (read-until
			    p (lambda (c)
				(if (eq? c #\space)
				    (begin
				      (set! loc-ended? #t)
				      #t)
				    (or (eof-object? c)
					(eq? c #\.)))))))
			 (maybe-loc-col
			  (if loc-ended? #f
			      (string->number (read-until
					       p (lambda (c)
						   (or (eof-object? c)
						       (eq? c #\space)))))))
			 (maybe-rest
			  ;; would want to accept "--message", too?
			  ;; But, strip space in normal case? But,
			  ;; lookahead ugly (even uglier than
			  ;; |loc-ended?| above?). Oh well, kiss I
			  ;; guess?
			  (if (maybe-read-string-expect p "-- ")
			      (read-line p)
			      #f)))
		    (list tag loc-file loc-line maybe-loc-col maybe-rest)))))))
    (cond ((test:string-starts-with? line "*** WARNING IN ")
	   (ra 'warning (substring* line 15)))
	  ((test:string-starts-with? line "ERROR ")
	   (ra 'error (substring* line 6)))
	  (else
	   (err "does not start with '*** WARNING IN ' or 'ERROR '")))))

;; and back to string...
(define (test:read-ignore-string base)
  (lambda (tag loc-file loc-line loc-col maybe-rest)
    (let ((l (make-location* loc-file (make-position* loc-line loc-col))))
      (case tag
	((error) (test:error-location-string l base))
	((warning) (test:warning-location-string l maybe-rest base))
	(else (error "bug"))))))

;; TESTs see end of file


(define (test:read-ignore path tail)
  (if (file-exists? path)
      (let ((tostring
	     (test:read-ignore-string
	      ;; (path-directory path) ah that won't work, relative or ""
	      ;; XX fix this mess, what location where ?
	      (current-directory))))
	(call-with-input-file path
	  (lambda (port)
	    (map/tail (lambda (line)
			(cons (apply tostring
				     (test:read-ignore-parse line))
			      #t))
		      tail
		      (read-lines port)))))
      tail))

(define (test:read-ignores . paths)
  (list->table (fold test:read-ignore
		     '()
		     (if (null? paths)
			 '(".test-ignore.txt")
			 paths))))


;; Run at test time for each TEST form:
(define (TEST:check res expect loc TEST:equal?)
  (set! TEST:repl-history
	(cons res ;; or expect ?
	      TEST:repl-history))
  ;; ^ XX use parameter? Currently not since reset on each TEST form.
  (if (TEST:equal? res expect)
      (parameter-inc! TEST:count-success)
      (if (test:ignore-warning?
	   (location-warn-to-string/normalize
	    loc path-expand "TEST failure, got" res))
	  (begin
	    (parameter-inc! TEST:count-fail-ignored)
	    (location-warn* loc "TEST failure, got" res))
	  (begin
	    (parameter-inc! TEST:count-fail)
	    (location-warn loc "TEST failure, got" res))))
  (void))

(define (TEST:repl-result-history-ref n)
  (list-ref TEST:repl-history n))

(both-times
 ;; caching path-normalize, primary reason being to avoid potential
 ;; size cost because of dubplicated strings in source files (speedup
 ;; only secondary)
 (define test:path-normalize-cache (make-table))
 (define (test:path-normalize path)
   (cond ((table-ref test:path-normalize-cache path #f)
	  => values)
	 (else
	  (let ((res (path-normalize path)))
	    (table-set! test:path-normalize-cache path res)
	    res))))
 
 (define (TEST:sourcepath->testfilepath path)
   ;; test:path-normalize to make sure that the testfilepath can be used
   ;; for comparisons, i.e. will always be the same for the same
   ;; source file however it is being accessed:
   (let ((npath (test:path-normalize path)))
     (let ((d (path-directory npath))
	   (n (path-strip-directory npath)))
       (string-append d ".test-" n)))))

;; Normal |write| cannot be used well because the file name in source
;; location objects will then be duplicated all over the place, which
;; not only bloats the files but also the need for memory. |write|
;; with structure sharing doesn't scale on Gambit. Thus use
;; object->u8vector instead--the output wouldn't have been readable
;; anyway, and using something like (map cj-desourcify (test-forms-for
;; "file")) is a better way to inspect the saved tests.

(both-times
 (define (TEST:write-form s p)
   (let* ((v (object->u8vector s))
	  (len (u8vector-length v)))
     (display len p)
     (newline p)
     (force-output p)
     (write-subu8vector v 0 len p)
     ;; unnecessary?:
     (force-output p))))

(define (TEST:read-form p cont end)
  (let ((lenstr (read-line p)))
    (if (eof-object? lenstr)
	(end)
	(let ((len (or (string->number lenstr)
		       (error "file format error"))))
	  (let ((v (##make-u8vector len)))
	    (or (= (read-subu8vector v 0 len p len) len)
		(error "some error reading"))
	    (cont (u8vector->object v)))))))

(define-macro* (TEST . forms)
  (if (pair? forms)
      (let ((sourcefile (container->path
			 (location-container (source-location (car forms))))))
	(if (not (string? sourcefile))
	    (error "can only use TEST from a file -- not from this:"
		   sourcefile))
	(let ((outfile (TEST:sourcepath->testfilepath sourcefile))
	      (writeit
	       (lambda (p)
		 (let ((loc (source-location (car forms)))
		       (code `(begin
				(define TEST:equal? equal?)
				(define TEST:repl-history '())
				(define (delete-repl-history!)
				  (set! TEST:repl-history '()))
				,@(TEST:conv forms stx))))
		   ;; normalize location-container here, too
		   (let ((location (make-location
				    (test:path-normalize
				     (container->path (location-container loc)))
				    (location-position loc))))
		     (cond ((table-ref TEST:seen location #f)
			    => (lambda (code0)
				 (if (not (equal? code0 code))
				     (error "multiple TEST expansion with differing results:"
					    code0 code))))
			   (else
			    (table-set! TEST:seen location code)
			    (TEST:write-form
			     (cj-sourcify-deep code stx)
			     p))))))))
	  (cond ((table-ref TEST:outports outfile #f)
		 => writeit)
		(else
		 (let ((p (open-output-file outfile)))
		   (table-set! TEST:outports outfile p)
		   (writeit p)))))
	;; at runtime, just register the file to make running all
	;; tests work: using Gambit namespaces here to ensure it works
	;; even if ##namespace is used somewhere
	`(set! TEST#loaded
	       (cons ,(test:path-normalize
		       (container->path (location-container
					 (source-location (car forms)))))
		     TEST#loaded)))
      '(begin)))

(define-macro* (TEST-disabledXX . args)
  `(begin))


(define (test-forms-for sourcefile)
  (let* ((testfile (TEST:sourcepath->testfilepath sourcefile)))
    (call-with-input-file (list path: testfile
				buffering: #f)
      (lambda (p)
	(let rec ()
	  (TEST:read-form
	   p
	   (lambda (form)
	     (cons form (rec)))
	   (lambda ()
	     '())))))))

(define TEST:count-success (make-parameter #f))
(define TEST:count-fail (make-parameter #f))
(define TEST:count-fail-ignored (make-parameter #f))
(define TEST:running (make-parameter #f))
(define test:current-test-location (make-parameter #f))
(define test:current-orig-handler (make-parameter #f))

(define (run-tests #!key (verbose #t)
		   #!rest files)
  (parameterize
   ((test:current-test-location #f)
    (TEST:count-success 0)
    (TEST:count-fail 0)
    (TEST:count-fail-ignored 0)
    (TEST:running #t)
    (current-test-ignores (test:read-ignores))
    (test:current-orig-handler (current-exception-handler)))
   (begin
     (let ((test-file
	    (lambda (file) ;; file is not normalized in case of manual input
	      (if verbose
		  (begin (display "Testing file ") (display file) (newline)))
	      (let lp ((forms (test-forms-for file))
		       (i 0))
		(if (null? forms)
		    (void)
		    (begin
		      (if verbose
			  (begin
			    (display "TEST form no. ") (display i) (newline)))

		      (cond
		       ((call/cc
			 (lambda (error-exit)
			   (with-exception-handler
			    (lambda (e)
			      (let ((l (test:current-test-location)))
				(if (and l
					 (test:ignore-error?
					  l
					  ;; XX CWD always right? Really
					  ;; what CWD was when loaded file,
					  ;; right?
					  (current-directory)))
				    ;; ignore it
				    (continuation-capture
				     (lambda (ctx)
				       (error-exit (vector e ctx l))))
				    ;; repl or whatever
				    ((test:current-orig-handler) e))))
			    (lambda ()
			      (eval (car forms))
			      #f))))
			=> (lambda (e.ctx.l)
			     (parameter-inc! TEST:count-fail-ignored)
			     ;; XXX + how many tests are being
			     ;; skipped by this?
			     (let ((e (vector-ref e.ctx.l 0))
				   (ctx (vector-ref e.ctx.l 1))
				   (l (vector-ref e.ctx.l 2)))
			       (display
				(string-append
				 "aborting in "
				 (location-string l
						  non-highlighting?: #t)
				 ":\n"))
			       (display
				(string-append
				 "*** Error in "
				 (location-string
				  (##continuation-locat ctx)
				  non-highlighting?: #t)
				 " -- "))
			       (display-exception e)
			       (newline)))))

		      (lp (cdr forms)
			  (inc i)))))))
	   (all-tests
	    (lambda ()
	      (let ((seen (make-table)))
		(let lp ((out '())
			 (lis TEST#loaded))
		  (if (null? lis)
		      out
		      (let ((a (car lis))
			    (r (cdr lis)))
			(if (table-ref seen a #f)
			    (lp out
				r)
			    (begin
			      (table-set! seen a #t)
			      (lp (cons a out)
				  r))))))))))

       (if (pair? files)

	   ;; first check if they are loaded
	   (let* ((files* (map perhaps-add-.scm files))
		  (loaded* (list->table
			    (map (lambda (f)
				   (cons f #t))
				 TEST#loaded)))
		  ;;^ loaded are normalized
		  (loaded? (lambda (file)
			     ;;(re test:path-normalize: heh I'm going to
			     ;;lenghts to throw the loaded-check error in
			     ;;tail position, then wrong paths will throw
			     ;;exceptions from the middle of the code
			     ;;anyway..)
			     (table-ref loaded* (test:path-normalize file) #f)))
		  (loaded (filter loaded? files*))
		  (not-loaded (filter (complement loaded?) files*)))

	     (if (pair? not-loaded)
		 (test:warn
		  "These files are not loaded or don't contain TEST forms:\n"
		  not-loaded))

	     (for-each test-file loaded))

	   ;; otherwise run all loaded:
	   (for-each test-file (all-tests))))

     (print (list (TEST:count-success) " success(es), "
		  (TEST:count-fail) " failure(s), "
		  (TEST:count-fail-ignored) " ignored failure(s)"
		  "\n")))))


;; testing TEST:

(TEST)
; (TEST
;  >)
; gives error
(TEST
 > '> ;; '>2 has to warn
 >)
(TEST
 > '>3 ;; must not warn, because it's understood as a sideeffecting form
 > 4)
(TEST
 > '>4 ;; (ditto)
 > 5
 5)


;; ignore file parser:
(TEST
 > (test:read-ignore-parse "*** WARNING IN \"/home/foo/lib/test.scm\"@623.4 -- TEST failure, got: >2")
 (warning "/home/foo/lib/test.scm" 623 4 "TEST failure, got: >2")
 ;; > (apply (test:read-ignore-string "/base") #)
 ;; "*** WARNING IN \"/home/foo/lib/test.scm\"@623.4 -- TEST failure, got: >2"
 ;; ah, not any more, now normalization is called
 > (test:read-ignore-parse "ERROR \"lib/test.scm\"@623.4")
 (error "lib/test.scm" 623 4 #f)
 ;; > (apply (test:read-ignore-string "/base") #)
 ;; "ERROR \"/base/lib/test.scm\"@623"
 ;;now getting: "ERROR \"../abcd/scheme/lib/test.scm\"@623"
 > (test:read-ignore-parse "ERROR \"/home/foo/lib/test.scm\"@623")
 (error "/home/foo/lib/test.scm" 623 #f #f)
 > (test:read-ignore-parse "ERROR \"lib/test.scm\"@623 -- 4")
 (error "lib/test.scm" 623 #f "4"))

