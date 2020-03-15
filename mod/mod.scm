;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(include "lib.scm")
(include "gambit.scm")

;; Data types:

;; modsym:   a symbol with dots for the namespace hierarchy:  match
;; modname:  a string with slash instead of the dots:  "lib/match"
;; mod:      modsym and possibly source code where it 'came from':
;;                  (match . #(source1 .....))
;; path:     path string to .scm file


(define-type mod
  id: 81947a55-3550-455d-951e-59b1195596fa
  sym
  maybe-from)

;; forgot how to specify for these directly?
(define mod.sym mod-sym)
(define mod.maybe-from mod-maybe-from)

(define (mod.name mod)
  (modsym.modname (mod.sym mod)))


(define (modname.path name)
  (string-append name ".scm"))

(define (modsym.modname sym)
  (list->string
   (map (lambda (c)
	  (if (char=? c #\.)
	      #\/
	      c))
	(string->list
	 (symbol->string sym)))))



;; Allow macros to inspect whether expansion is happening for
;; compilation through C

(define mod:compiled?
   (make-parameter #f))


;; possibly-compile and load

;; BUGS:

;; - sometimes when loading compiled object, editing file, reloading
;; into running system it will compile and load, then on subsequent
;; load give the 'cannot load multiple times' error

;; - sometimes, with (define compile-mode 'c), a file that has been
;; compiled, when edited, will not get reloaded into the running
;; system from source, neither freshly compiled. strange. Only time
;; this happened was with Serialization-Deserialization.scm

(compile-time-define-if-not-defined objects-loaded (make-table))
;; name to [file mtime,no,] index

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



;; -----------------------


(define (mod.maybe-last-objectfile mod)
  (let ((name (mod.name mod)))
    (let lp ((i 1)
	     (prev-p #f))
      (let ((p (string-append name ".o"
			      (number->string i))))
	(if (file-exists? p)
	    (lp (+ i 1) p)
	    prev-p)))))

(define (i/load name)
  (let ((depends+code (mod.depends+code name)))
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
     (let ((sourcefile (modname.path name)))
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
	 (cond (ç mod.maybe-last-objectfile well +i
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
  '(srfi-1
    cj-env-1
    vector-util-1
    list-util-1
    cj-source))



;; STATI:
;; - already loaded, no change  neither in the file nor its dependencies
;; - already loaded, no change  except some dependencies changed -> reloadorcompile
;; - already loaded, file changed -> reloadorcompile
;; - not loaded: obj file matches source file -> process deps, then load obj file
;; - not loaded: obj file doesn't match source file -> process deps, then loadorcompile

;; --- load state ----

(define mod-loaded #f) ;; sym -> statically|loading|loaded

(define (init-mod-loaded!)
  (set! mod-loaded (make-table test: eq?))
  (for-each (lambda (sym)
	      (table-set! mod-loaded sym (make-parameter 'statically)))
	    mod:statically-loaded))

(init-mod-loaded!)

(define (mod.loaded? mod)
  (let ((sym (mod.sym mod)))
    (cond ((table-ref mod-loaded sym #f)
	   => (lambda (v)
		(case (v)
		  ((loaded statically)
		   #t)
		  ((loading)
		   (raise-source-error (mod.maybe-from mod)
                                       "circular dependency on" sym))
		  ((#f)
		   ;; left-over parameter object
		   #f)
		  (else
		   (error "bug")))))
	  (else
	   #f))))

(define (mod.loaded mod)
  ;; always returns parameter object, creating it as a side effect if
  ;; not already existing
  (let ((sym (mod.sym mod)))
    (or (table-ref mod-loaded sym #f)
	(let ((p (make-parameter #f)))
	  (table-set! mod-loaded sym p)
	  p))))

(define (call-with-mod-loading mod thunk)
  (parameterize (((mod.loaded mod) 'loading))
		(thunk)))


;; --- loading & compilation -----

;; 'cload' means, get the current code into the address space of the
;; process, and if necessary first compile it.

(define (mod.maybe-cload/deps mod)
  ;; returns true if mod was [re]loaded
  (call-with-mod-loading
   mod
   (lambda ()
     ;; returns a continuation to be called after exiting the
     ;; 'loading' scope
     (let* ((dep-changed? (fold (lambda (mod dep-changed?)
				  (or (mod.maybe-cload/deps mod)
				      dep-changed?))
				#f
				(modname.depends (mod.name mod)))))
       (if (or (not (mod.loaded? mod))
	       (mod.changed? mod)
	       dep-changed?)
	   (mod.cload mod))))))

(define (mod.want-compilation? mod)
  (not (symbol-memq (mod.sym mod) interpreted-modules)))

;; only call once it's decided that a module needs to be reloaded
;; (incl. perhaps recompiled)
(define (mod.cload mod)
  (fourmi-run cload-thread 'cload mod))


;; To be run as singleton instance in a separate thread: Run 'load'
;; and dispatch of compilation requests from a single thread only.
(define cload-thread
  (fourmi-server
   (lambda (cmd)
     (case cmd
       ((cload)
	(lambda (return mod)
	  (let* ((load*
		  ;; Careful: don't use this from directly within the
		  ;; cload-thread itself
		  (lambda (path)
		    (fourmi-run cload-thread 'load path)))
		 (compile-and-load
		  (thunk
		   (future
		    (return (load* (mod.compile mod)))))))
	    ;; 'business logic':
	    (if (mod.wants-compilation? mod)
		(cond ((mod.maybe-last-objectfile mod)
		       => (lambda (objectfile)
			    (if (>= (file-mtime (mod.sourcefile mod))
				    (file-mtime objectfile))
				;;çXX hm schon gecheckt oben ob neuer bei maybe-cload.halb.
				(compile-and-load)
				(return (load objectfile)))))
		      (else
		       (compile-and-load)))
		(return (load (mod.sourcefile mod)))))))
       ((load)
	(lambda (return path)
	  (return (load path))))
       ((quit)
	(lambda _
	  (return 'OK)
	  (quit)))
       (else
	(lambda _
	  (error "invalid command:" cmd)))))))


;; once it's decided that mod needs compilation, send compilation
;; request to compilation dispatcher and wait for the result.
(define (mod.compile mod)
  )



;; (define (modsym.maybe-cload sym)
;;   (mod.maybe-cload/deps (make-mod sym #f)))


;;çç OLD:
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
	      ((if ç
		   i/load
		   ;;ç c:
		   i/load)
	       (modsym.modname sym))
	      (table-set! mod-loaded sym 'loaded))
	    (lambda ()
	      (if (eq? (table-ref mod-loaded sym #f) 'loading)
		  (table-set! mod-loaded sym)
		  ;; or set as 'error ? But intention is to let user
		  ;; retry from repl.
		  ))))))

;; --- file parsing -----

(define (modname.depends+rcode name)
  (fold (lambda (form depends+rcode)
	  (let ((depends (car depends+rcode))
		(rcode (cdr depends+rcode)))
	    (cond ((mod:form->maybe-requires-imports form)
		   => (lambda (imports)
			(cons (map/tail (lambda (import)
					  (make-mod
					   (mod:require-import->mod import)
					   import))
					depends
					imports)
			      rcode)))
		  (else
		   (cons depends
			 (cons form rcode))))))
	(cons '() '())
	(call-with-input-file (modname.path name) read-all-source)))

(define (modname.depends+code name)
  (let* ((depends+rcode (modname.depends+rcode name))
	 (depends (car depends+rcode))
	 (exprs (reverse (cdr depends+rcode))))
    (cons depends
	  ;; XX: only the cons and 'begin need source info here, could
	  ;; save the deep
	  (cj-sourcify-deep (cons 'begin exprs)
			    (car exprs)))))


;; XXX inefficient, cache somehow
(define (modname.depends name)
  (car (modname.depends+code name)))


(define (mod:form->maybe-requires-imports stx #!optional ignore-head?)
  (let* ((stx* (source-code stx)))
    (and (pair? stx*)
	 (or ignore-head?
	     (eq? (source-code (car stx*)) 'require))
	 (cdr stx*))))


(define (mod:require-import->mod import)
  (let* ((import* (source-code import))
	 (mod (if (pair? import*)
		  (if (null? (cdr import*))
		      (car import*)
		      (raise-source-error import "invalid require syntax"))
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


;; --- ~'main' -----

(include "usersyntax.scm")

;; load config
;; XX: just as in the require form, assumes that this repo is
;; accessible at lib/
(load "lib/mod/config")
