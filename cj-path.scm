;;; Copyright 2013-2020 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         C
	 string-util-2
	 (string-util-1 string-contains-char?)
         (cj-functional-2 both either)
         (srfi-1 list-index)
         test)

(export path-string?
        ;; for port-settings see port-settings.scm
        filename-or-.-..-string?
        filename-string?
        path-separator
        string-ends-with-path-separator?
        path-absolute?
        path-append
        if-file-info ;; move?
        maybe-file-info
        file-directory?
        -d?
        -f?
        (macro FILE))


"Unix path handling; not parametrized at all for Windows currently."


(define (path-string? v)
  (and (string? v)
       (not (string-empty? v))
       ;; and the only(?) restriction:
       (not (string-contains-char? v (C char=? _ #\nul)))))

(TEST
 > (path-string? "foo")
 #t
 > (path-string? "foo\0")
 #f
 > (path-string? "")
 #f)


(define (filename-or-.-..-string? v)
  (and (path-string? v)
       ;; max length is (a) filesystem dependent, and (b) encoding
       ;; dependent. More than 255 characters shouldn't be there,
       ;; though, 'usually'.
       (<= 1 (string-length v) 255)
       (not (string-contains? v "/"))))

(define (filename-string? v)
  (and (filename-or-.-..-string? v)
       (not (string=? v "."))
       (not (string=? v ".."))))


(TEST
 > (filename-string? ".")
 #f
 > (filename-string? "..")
 #f
 > (filename-or-.-..-string? ".")
 #t
 > (filename-or-.-..-string? "..")
 #t
 > (filename-or-.-..-string? "/")
 #f
 > (filename-or-.-..-string? "a..")
 #t
 > (filename-or-.-..-string? "..a")
 #t
 > (filename-string? "..a")
 #t
 > (filename-string? "a")
 #t
 > (filename-string? "./a")
 #f
 > (filename-string? "")
 #f
 > (filename-string? "foo\0")
 #f)




;; (define (path-append a b)
;;   (string-append a "/" b))


(define path-separator "/")

(define string-ends-with-path-separator?
  (C string-ends-with? _ path-separator))

(define path-absolute?
  (C string-starts-with? _ path-separator))

(define (path-append basepath subpath)
  (if (path-absolute? subpath)
      subpath
      (string-append basepath
		     (if (string-ends-with-path-separator? basepath)
			 ""
			 path-separator)
		     subpath)))





;; XX should this be in an IO library, not path.

(define (if-file-info path chase? then/1 else/0)
  (let ((prevhandler (current-exception-handler)))
    (continuation-capture
     (lambda (return)
       (then/1 (with-exception-handler
		(lambda (e)
		  (if (no-such-file-or-directory-exception? e)
		      (continuation-graft return else/0)
		      (prevhandler e)))
		(thunk
		 (file-info path chase?))))))))

(define (maybe-file-info path #!optional (chase? #t))
  (if-file-info path chase? identity false/0))

(define (file-directory? path)
  (eq? (file-info-type (file-info path)) 'directory))
;; well, better, no error when not existing:
(define (-d? path #!optional (chase? #t))
  (if-file-info path
		chase?
		(lambda (info)
		  (eq? (file-info-type info) 'directory))
		false/0))
(define (-f? path #!optional (chase? #t))
  (if-file-info path
		chase?
		(lambda (info)
		  (eq? (file-info-type info) 'regular))
		false/0))


(define-macro* (FILE)
  "The path to the file the macro is used in; #f if not a
file (e.g. console)."
  (let ((c (location-container (source-location stx))))
    (and (string? c)
         c)))


