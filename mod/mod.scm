;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; possibly-compile and load

;; BUGS:

;; - sometimes when loading compiled object, editing file, reloading
;; into running system it will compile and load, then on subsequent
;; load give the 'cannot load multiple times' error

;; - sometimes, with (define compile-mode 'c), a file that has been
;; compiled, when edited, will not get reloaded into the running
;; system from source, neither freshly compiled. strange. Only time
;; this happened was with Serialization-Deserialization.scm

(define-macro (compile-time-define-if-not-defined name expr)
  (with-exception-catcher
   (lambda (e)
     (eval `(define ,name ,expr)))
   (lambda ()
     (eval name) ;; if it doesn't exist, will define it
     '(begin))))
(compile-time-define-if-not-defined objects-loaded (make-table)) ;; name to [file mtime,no,] index
(define (object-load-if-changed name i)
  (define (load+set)
    (load name)
    (table-set! objects-loaded name i))
  (if (eq? compile-mode 's)
      (error "BUG"))
  (cond ((table-ref objects-loaded name #f)
	 => (lambda (oldi)
	      (if (> i oldi)
		  (load+set)
		  #f)))
	(else
	 (load+set))))

(define mod:compiled?
   (make-parameter #f))

(define (i/load name)
  (let ((sourcefile (string-append name ".scm")))
    (load sourcefile)))

(define (c/load name)
  ;; possibly compile and load:
  (let ((sourcefile (string-append name ".scm")))
    (case compile-mode
      ((s) ;; always source
       ;;XXX shouldn't this be i/load ?:
       (load sourcefile))
      (else
       (let* ((sourceinf (file-info sourcefile))
	      (evtl-compile+load
	       (lambda (i)
		 (case compile-mode
		   ((l) ;; load binary or source, whatever is newer (left up to Gambit)
		    (error "not yet implemented"))
		   ((c) ;; compile
		    (println (list "compiling: " name))
		    (parameterize
		     ((mod:compiled? #f))
		     (apply compile-file sourcefile compile-options))
		    ;; gives #f on failure; but, we want to go to the debugger
		    ;; maybe?, or at least stop the process, so:
		    (object-load-if-changed name i))
		   (else
		    (error "invalid compile-mode value:" compile-mode))))))
	 (cond ((let lp ((i 1)
			 (obinf #f))
		  (let ((obp (string-append name ".o"
					    (number->string i))))
		    (if (file-exists? obp)
			(lp (+ i 1)
			    (file-info obp))
			(and obinf
			     (cons obinf (- i 1))))))
		=>
		(lambda (obinf+i)
		  (if (> (time->seconds (file-info-last-modification-time sourceinf))
			 (time->seconds (file-info-last-modification-time (car obinf+i))))
		      (evtl-compile+load (cdr obinf+i))
		      ;; assuming macros haven't changed
		      (object-load-if-changed name (cdr obinf+i)))))
	       (else
		;; no binary
		(evtl-compile+load 1))))))))


(include "../cj-source.scm")


(define (mod->mod-path sym)
  (list->string
   (map (lambda (c)
	  (if (char=? c #\.)
	      #\/
	      c))
	(string->list
	 (symbol->string sym)))))

;; load interpreted if in list of to-be interpreted modules, otherwise
;; compiled, and do it only once per reload request (see |R| below)

(define mod-loaded #f)
(define (init-mod-loaded!)
  (set! mod-loaded (make-table test: eq?)))
(init-mod-loaded!)
(define (mod-loaded? sym)
  (table-ref mod-loaded sym #f))
(define (mod-load sym)
  (if (not (mod-loaded? sym))
      (begin
	;; set it first, to avoid cycles during loading
	(table-set! mod-loaded sym #t)
	((if (memq sym interpreted-modules)
	     i/load
	     c/load)
	 (mod->mod-path sym)))))

(define (require-import->mod import)
  (let* ((import* (source-code import))
	 (mod (if (pair? import*)
		  (if (null? (cdr import*))
		      (car import*)
		      ;; XXX source-error
		      (error "invalid syntax" import))
		  import*))
	 (mod* (source-code mod)))
    (if (symbol? mod*)
	mod*
	(error "expecting symbol" mod))))

(define (require-expand stx)
  ;; would like to have access to improper-* functions. well
  (cj-sourcify-deep
   (let* ((stx* (source-code stx))
	  (a (car stx*))
	  (imports (cdr stx*)))
     (cons 'begin
	   (map (lambda (import)
		  `(mod-load ',(require-import->mod import)))
		imports)))
   stx))

;; |require| runtime macro
(##top-cte-add-macro!
 ##interaction-cte
 'require
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    (let ((code (require-expand stx)))
      (eval code)
      code))
  #f))

;; |RQ|, a require for user interaction that deletes the loaded table
;; first
(##top-cte-add-macro!
 ##interaction-cte
 'RQ
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    (init-mod-loaded!)
    (require-expand stx))
  #f))


;; load config
;; XX: just as in the require form, assumes that this repo is
;; accessible at lib/
(load "lib/mod/config")
