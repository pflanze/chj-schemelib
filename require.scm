;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)


(define (scm-stripsuffix p)
  (let ((len (string-length p)))
    (if (and (> len 4)
	     (string=? (substring p (- len 4) len) ".scm"))
	(substring p 0 (- len 4))
	(error "not a path with suffix '.scm':" p))))

(define (simple-basename p)
  (let ((len (string-length p)))
    (let lp ((i (- len 1)))
      (if (negative? i)
	  p
	  (let ((c (string-ref p i)))
	    (if (char=? c #\/)
		(substring p (+ i 1) len)
		(lp (- i 1))))))))

;; unlike (basename p ".scm"), this also complains for wrong suffix
(define (scm-basename p)
  (simple-basename (scm-stripsuffix p)))


(define (require-decl.modulename v)
  (cond ((symbol? v)
	 (source-code v))
	((pair? v)
	 (source-code (car (source-code v))))
	(else
	 (error "no match:" v))))


;; expect relative path, strip first level of folders. XX this is a
;; HACK to get rid of project folders; but local subfolders will be
;; stripped as well.

;; (define (path-string.modulename p) ;; -> symbol?
;;   (let ((l (string-split (scm-stripsuffix p) #\/)))
;;     (if (string=? (car l) "")
;; 	(error "need relative path, got:" p)
;; 	(strings-join (cdr l) "/"))))
;; ^ relies on unavailable dependencies

(define (path-string.modulename p) ;; -> symbol?
  (string->symbol
   (let* ((s (scm-stripsuffix p))
	  (len (string-length s))
	  (maybe-ifirst (let lp ((i 0))
			  (if (< i len)
			      (if (char=? (string-ref s i) #\/)
				  i
				  (lp (+ i 1)))
			      #f))))
     (if maybe-ifirst
	 (if (zero? maybe-ifirst)
	     (error "need relative path, got:" p)
	     (substring s (+ maybe-ifirst 1) len))
	 s))))


(define modules-without-require-forms
  '( ;; cj-source
    ;; define-macro-star
    ;; dummy-module
    vector-util-1
    cj-env-1

    ;; mydb top
    config

    ;; lib/mod/ :
    config-example
    gambit
    imperative-load-tree
    lib
    mod
    monad
    monadic-load-tree
    remote
    usersyntax))

(define (path-string.relation p relation)
  (let ((form (call-with-input-file p read))
	(mname (path-string.modulename p)))
    (let rec ((form form))
      (cond
       ;; `(require . `rest)
       ((and (pair? form)
	     (eq? (car form) 'require))
	(relation mname
		  (map require-decl.modulename (cdr form))))

       ;; `(quote `q)
       ((and (pair? form)
	     (eq? (car form) 'quote)
	     (pair? (cdr form))
	     (null? (cddr form)))
	(rec (cadr form)))

       (else
	(if (memq mname modules-without-require-forms)
	    (relation mname '())
	    (error "file does not have a require form as its first form:"
		   p)))))))

;; tests see in require-util.scm
