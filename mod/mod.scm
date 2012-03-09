;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Data types:

;; sym:   a symbol with dots for the namespace hierarchy:  lib.match
;; name:  a string with slash instead of the dots:  "lib/match"
;; mod:   module possibly including source code where it 'comes from':
;;                  (lib.match . #(source1 .....))


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

(define (mod:name->path name)
  (string-append name ".scm"))


(define (F v)
  (let ((v (force v)))
    (cond ((pair? v)
	   (cons (F (car v))
		 (F (cdr v))))
	  (else
	   v))))


(include "monadic-load-tree.scm")
;; (include "imperative-load-tree.scm")
(include "remote.scm")


(include "local.scm")

(define (compile-expr path expr)
  ;; reuses global compile-options
  (local ((c#expand-source (lambda (_expr)
			     expr)))
	 (compile-file path compile-options)))


;; -----------------------

(define (i/load name)
  (let ((depends+code (mod:file->depends+code name)))
    (for-each (lambda (depend)
		(mod-load (car depend) (cdr depend)))
	      (car depends+code))
    (println "- evalling code of " name);;ç
    (eval (cdr depends+code))))

(define (c/load name)
  ;; possibly compile and load:
  (case compile-mode
    ((s) ;; always source
     (i/load name))
    (else
     (let ((sourcefile (mod:name->path name)))
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
		  (if (>= (file-mtime sourceinf)
			  (file-mtime (car obinf+i)))
		      (evtl-compile+load (cdr obinf+i))
		      ;; assuming macros haven't changed
		      (object-load-if-changed name (cdr obinf+i)))))
	       (else
		;; no binary
		(evtl-compile+load 1))))))))

(define (file-mtime path)
  (time->seconds (file-info-last-modification-time path)))

;; in the (for bootstrapping) manually included files, ignore require
;; forms
(define-macro (require . args)
  '(begin))

(include "../srfi-1.scm")
(include "../cj-env-1.scm")
(include "../vector-util-1.scm")
(include "../list-util-1.scm")
(include "../cj-source.scm")
(define mod:statically-loaded ;; keep in sync with the above!
  '(lib.srfi-1
    lib.cj-env-1
    lib.vector-util-1
    lib.list-util-1
    lib.cj-source))

(define (mod:file->depends+rcode name)
  (fold (lambda (form depends+rcode)
	  (let ((depends (car depends+rcode))
		(rcode (cdr depends+rcode)))
	    (cond ((mod:form->maybe-requires-imports form)
		   => (lambda (imports)
			(cons (map/tail (lambda (import)
					  (cons (mod:require-import->mod import)
						import))
					depends
					imports)
			      rcode)))
		  (else
		   (cons depends
			 (cons form rcode))))))
	(cons '() '())
	(call-with-input-file (mod:name->path name) read-all-source)))

(define (mod:file->depends+code name)
  (let* ((depends+rcode (mod:file->depends+rcode name))
	 (depends (car depends+rcode))
	 (exprs (reverse (cdr depends+rcode))))
    (cons depends
	  ;; XX: only the cons and 'begin need source info here, could
	  ;; save the deep
	  (cj-sourcify-deep (cons 'begin exprs)
			    (car exprs)))))


;; XXX inefficient, cache somehow
(define (mod:depends name)
  (map car (car (mod:file->depends+code name))))


;; STATI:
;; - already loaded, no change  neither in the file nor its dependencies
;; - already loaded, no change  except some dependencies changed -> reloadorcompile
;; - already loaded, file changed -> reloadorcompile
;; - not loaded: obj file matches source file -> process deps, then load obj file
;; - not loaded: obj file doesn't match source file -> process deps, then loadorcompile





(define (mod:form->maybe-requires-imports stx #!optional ignore-head?)
  (let* ((stx* (source-code stx)))
    (and (pair? stx*)
	 (or ignore-head?
	     (eq? (source-code (car stx*)) 'require))
	 (cdr stx*))))

(define (mod->mod-path sym)
  (list->string
   (map (lambda (c)
	  (if (char=? c #\.)
	      #\/
	      c))
	(string->list
	 (symbol->string sym)))))


;;; mod-load
;; load interpreted if in list of to-be interpreted modules, otherwise
;; compiled, and do it only once per reload request (see |R| below)

(define mod-loaded #f) ;; sym -> statically|loading|loaded

(define (init-mod-loaded!)
  (set! mod-loaded (make-table test: eq?))
  (for-each (lambda (sym)
	      (table-set! mod-loaded sym 'statically))
	    mod:statically-loaded))

(init-mod-loaded!)

(define (mod-loaded? sym stx)
  (cond ((table-ref mod-loaded sym #f)
	 => (lambda (v)
	      (case v
		((loaded)
		 #t)
		((loading)
		 (source-error stx "circular dependency on" sym)))))
	(else
	 #f)))

(define (mod-load sym stx)
  (if (not (mod-loaded? sym stx))
      (begin
	(println "- loading " sym)
	(dynamic-wind
	    ;;ç sollte ins c/load usw moved werden !  wenn jene user accsblrg
	    (lambda ()
	      ;; set to true value already, to avoid cycles during loading
	      (table-set! mod-loaded sym 'loading))
	    (lambda ()
	      ((if (memq sym interpreted-modules)
		   i/load
		   ;;ç c:
		   i/load)
	       (mod->mod-path sym))
	      (table-set! mod-loaded sym 'loaded))
	    (lambda ()
	      (if (eq? (table-ref mod-loaded sym #f) 'loading)
		  (table-set! mod-loaded sym)
		  ;; or set as 'error ? But intention is to let user
		  ;; retry from repl.
		  ))))))

(define (mod:require-import->mod import)
  (let* ((import* (source-code import))
	 (mod (if (pair? import*)
		  (if (null? (cdr import*))
		      (car import*)
		      (source-error import "invalid require syntax"))
		  import*))
	 (mod* (source-code mod)))
    (if (symbol? mod*)
	mod*
	(error "expecting symbol" mod))))


(define (mod:require-expand stx)
  ;; would like to have access to improper-* functions. well
  (cj-sourcify-deep
   (cond ((mod:form->maybe-requires-imports stx #t)
	  =>
	  (lambda (imports)
	    (cons 'begin
		  (map (lambda (import)
			 `(mod-load ',(mod:require-import->mod import)
				    (source-dequote ',(source-quote import))))
		       imports))))
	 (else
	  (error "not a require form")))
   stx))


;;; |require| runtime macro

(##top-cte-add-macro!
 ##interaction-cte
 'require
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    ;; do not eval at expansion time, because Gambit crashes when
    ;; doing nested compilation; instead usually require forms are
    ;; translated separately
    (location-warn
     (source-location stx)
     "fall back to macro definition of require form, no compile-time definitions are supported")
    (mod:require-expand stx))
  #f))


;;; |RQ|
;; a require for user interaction that first clears what has been loaded

(##top-cte-add-macro!
 ##interaction-cte
 'RQ
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    (init-mod-loaded!)
    (mod:require-expand stx))
  #f))


;; load config
;; XX: just as in the require form, assumes that this repo is
;; accessible at lib/
(load "lib/mod/config")
