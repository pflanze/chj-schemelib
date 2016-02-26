;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

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


(require (define-macro-star)
	 (cj-phasing)
	 (cj-env-1)
	 (lib.simple-match-1))

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


(compile-time
 (define-macro (TEST . args) `(begin)) ;; why do I have to (re?)add these? did I clean that up and now readding I guess

 (define TEST:outports (make-table))
 ;; to suppress double outputs:
 (define TEST:seen (make-table))

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

 
 (define (TEST:conv forms stx)
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
		      (cons expr
			    (rec/rest rest)))))
	       (match-list*
		rest
		(() (understand-as-sideeffect))
		((next . rest)
		 ;; btw O(n^2) because of rest scanning for proper list right. should have match* for that right?.
		 (let ((understand-as-test
			(lambda ()
			  (let ((result next))
			    (cons (cj-sourcify-deep
				   `(TEST:check (let ((repl-result-history-ref TEST:repl-result-history-ref))
						  ,@(if maybe-namespace-form
							(list maybe-namespace-form)
							(list))
						  ,expr)
						',result
						',(source-location expr)
						TEST:equal?)
				   expr)
				  (rec/rest rest))))))
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

(define (TEST:check res expect loc TEST:equal?)
  (define-macro (inc! v)
    `(set! ,v (inc ,v)))
  (set! TEST:repl-history
	(cons res ;; or expect ?
	      TEST:repl-history))
  (if (TEST:equal? res expect)
      (inc! TEST:count-success)
      (begin
	(inc! TEST:count-fail)
	(location-warn loc "TEST failure, got" res))))

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

(compile-time
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
                   (let ((sourcefile (container->path (location-container (source-location (car forms))))))
                     (if (not (string? sourcefile))
                         (error "can only use TEST from a file -- not from this:" sourcefile))
                     (let ((outfile (TEST:sourcepath->testfilepath sourcefile))
                           (writeit (lambda (p)
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
                                                          (error "multiple TEST expansion with differing results:" code0 code))))
                                                (else
                                                 (table-set! TEST:seen location code)
                                                 (TEST:write-form (cj-sourcify-deep code stx) p))))))))
                       (cond ((table-ref TEST:outports outfile #f)
                              => writeit)
                             (else
                              (let ((p (open-output-file outfile)))
                                (table-set! TEST:outports outfile p)
                                (writeit p)))))
                     ;; at runtime, just register the file to make running all tests work:
                     ;; using Gambit namespaces here to ensure it works even if ##namespace is used somewhere
                     `(set! TEST#loaded
                            (cons ,(test:path-normalize
                                     (container->path (location-container
                                                        (source-location (car forms)))))
                                  TEST#loaded)))
               '(begin)))

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

(define TEST:count-success #f)
(define TEST:count-fail #f)
(define TEST:running #f)

(define (run-tests #!key (verbose #t)
		   #!rest files)
  (set! TEST:count-success 0)
  (set! TEST:count-fail 0)
  (set! TEST:running #t)
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
		       (begin (display "TEST form no. ") (display i) (newline)))
		   (eval (car forms))
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
	      (warn "These files are not loaded or don't contain TEST forms:\n"
		    not-loaded))
	  (for-each test-file loaded))
	;; otherwise run all loaded:
	(for-each test-file (all-tests))))
  (print (list TEST:count-success " success(es), "
	       TEST:count-fail " failure(s)" "\n"))
  (set! TEST:running #f))


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
 > '>4 ;; (dito)
 > 5
 5)

