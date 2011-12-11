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

(define (mod:name->path name)
  (string-append name ".scm"))

;; ((mfn v) m) -> (cons v* m*)
;; (mfn v) -> m -> (cons v* m*)
;; (define (m:map mfn l)
;;   (lambda (m)
;;     ))

(define vals cons)
(define val0 car)
(define val1 cdr)

(define v+m vals)
(define val-v val0)
(define val-m val1)

(define (m:map mfn l m)
  (let rec ((l l)
	    (m m))
    (delay
      (if (null? l)
	  (v+m l m)
	  (let* ((v (force (mfn (car l) m)))
		 (w (force (rec (cdr l) (val-m v)))))
	    (v+m (cons (val-v v)
		       (val-v w))
		 (val-m w)))))))

;; (define (m:inc v m)
;;   (v+m (cons (inc v) m)
;;        (* m 2)))
;; > (m:map m:inc '(1 2 3 4) 10)
;; (((2 . 10) (3 . 20) (4 . 40) (5 . 80)) . 160)

(define (loaded+loading loaded loading)
  (vals loaded loading))
(define loadstate-loaded val0)
(define loadstate-loading val1)

(define (m:load-tree mod loadstate)
  (delay
    (let ((sym (val0 mod)))
      (cond ((memq sym (loadstate-loading loadstate))
	     (source-error (val1 mod) "circular dependency"
			   (loadstate-loading loadstate)))
	    ((memq sym (loadstate-loaded loadstate))
	     ;; ref, how.
	     (v+m `(loaded ,sym) loadstate))
	    (else
	     (let* ((depends+code
		     (begin
		       (println "calculating depends+code for:" sym)
		       (mod:file->depends+code (mod->mod-path sym))))
		    (v+loadstate*
		     (force
		      (m:map m:load-tree
			     ;; perhaps keep code?XX
			     (val0 depends+code)
			     ;; loading ourselves:
			     (loaded+loading
			      (loadstate-loaded loadstate)
			      (cons sym (loadstate-loading loadstate))))))
		    (v (val-v v+loadstate*))
		    (loadstate* (val-m v+loadstate*)))
	       (or (eq? (car (loadstate-loading loadstate*)) sym)
		   (error "bug"))
	       (v+m (cons sym v)
		    ;; loaded ourselves:
		    (loaded+loading
		     (cons sym (loadstate-loaded loadstate*))
		     (cdr (loadstate-loading loadstate*))))))))))


;; XX without locking or processes yet:
(define xbox-get! unbox)
;;(define xbox-set! set-box!)
(define (xbox-update! b fn)
  (set-box! b (fn (unbox b))))
(define make-xbox box)

(define (F v)
  (let ((v (force v)))
    (cond ((pair? v)
	   (cons (F (car v))
		 (F (cdr v))))
	  (else
	   v))))

;; (is |loading| necessary? vs just thread deadlock?)

(define load-tree/loading/save-code!
  (lambda (save-code! reference)
    (let ((refs (make-xbox '())))
      (letrec
	  ((load-tree/loading
	    (lambda (loading)
	      (lambda (mod)
		(delay
		  (let ((sym (val0 mod)))
		    (cond ((memq sym loading)
			   (source-error (val1 mod) "circular dependency"
					 loading))
			  ((assq sym (xbox-get! refs))
			   => reference)
			  (else
			   (let* ((depends+code
				   (begin
				     (println "calculating depends+code for:" sym)
				     (mod:file->depends+code (mod->mod-path sym))))
				  (res
				   (cons sym
					 (map (load-tree/loading
					       (cons sym loading))
					      ;; perhaps keep code?XX
					      (val0 depends+code)))))
			     (xbox-update! refs (lambda (l)
						  (cons (cons sym res) l)))
			     (save-code! sym (val1 depends+code))
			     res)))))))))
	load-tree/loading))))

(define (load-tree mod)
  (((load-tree/loading/save-code!
     (lambda (sym code)
       (void))
     (lambda (k+v)
       `(REF ,(cdr k+v))))
    '())
   mod))

;; data types:
;;  commport  just the open-process port
;;  remcomm   bundled with local vector port for receiving (commport only send)

(define make-remcomm cons)
(define (remcomm:remote-port rc)
  (car rc))
(define (remcomm:vector-port rc)
  (cdr rc))

(define (start-compiler)
  (let ((p (open-process
	    (list path: "gsc"
		  arguments: (list
			      "-:tE,d-,t8,f8,tu"
			      ;; "-f" ;; no .gambcini loading
			      ;; XX ^ actually load code that way for now
			      )
		  stdin-redirection: #t
		  stdout-redirection: #t
		  ;;XX: bad bc of loosing ordering. but, would need sep ports
		  stderr-redirection: #f))))
    (write `(remote:start-compiler) p)
    (force-output p)

    ;; Drop Gambit welcome message
    (let lp ()
      (let ((line (read-line p #\newline #f)))
	(if (string=? line "compiler-running")
	    (void)
	    (lp))))

    (let ((remcomm (make-remcomm p (open-vector (list buffering: #f)))))
      ;; start proxy thread
      (thread-start!
       (make-thread
	(commport-dispatcher remcomm)))

      (dorem:check-ok remcomm)

      remcomm)))

(define (stop-compiler p)
  (remcomm:send p `(exit))
  (close-port p)
  (process-status p))


;; run aynchronically (but sends normal messages back to 'synchronous'
;; thread)
(define (commport-dispatcher remcomm)
  (let ((rp (remcomm:remote-port remcomm))
	(vp (remcomm:vector-port remcomm)))
    (lambda ()
      (let lp ()
	(let ((msg (remcomm:recv rp)))
	  (case (and (pair? msg)
		     (car msg))
	    ((port)
	     ;; heh and here it's *not* a separate thread
	     (println (cadr msg) ": " (caddr msg)))
	    (else
	     ;; send  ?  why write?
	     (write msg vp))))
	(lp)))))

(define (make-dorem-command format cont)
  (lambda (p . args)
    (let ((rp (remcomm:remote-port p))
	  (vp (remcomm:vector-port p)))
      ;; v--XX ach wll wrong name for remcomm:send then. sgh
      (remcomm:send rp (apply format args))
      (let redo ()
	;; (why read and not a recv)
	(let ((msg (read vp)))
	  (case (and (pair? msg)
		     (car msg))
	    ((value)
	     (cont (cadr msg)))
	    ((exception)
	     (raise (cadr msg)))
	    (else
	     (error "invalid reply:" msg))))))))

(define dorem:check-ok
  (make-dorem-command
   (lambda ()
     `(ok?))
   (lambda (reply)
     (or (eq? reply 'ok)
	 (error "unexpected reply:" reply)))))

(define dorem:check-ok-error
  (make-dorem-command
   (lambda ()
     `(ok-error))
   values))

(define dorem:compile-expr
  (make-dorem-command
   (lambda (path expr)
     `(compile-expr ,path ,expr))
   values))

(define (natural0->u8vector n len)
  (let ((v (make-u8vector len)))
    (let lp ((i 0)
	     (r n))
      (if (zero? r)
	  v
	  (if (< i len)
	      (let* ((r* (arithmetic-shift r -8))
		     (b (bitwise-and r 255)))
		(u8vector-set! v i b)
		(lp (inc i)
		    r*))
	      (error "number does not fit in len:" n len))))))

;; > (natural0->u8vector 255 4)
;; #u8(255 0 0 0)
;; > (natural0->u8vector 256 4)
;; #u8(0 1 0 0)
;; > (natural0->u8vector 258 4)
;; #u8(2 1 0 0)
;; > (natural0->u8vector 123210391823 5)
;; #u8(15 97 233 175 28)
;; > (+ 15 (* 256 (+ 97 (* 256 (+ 233 (* 256 (+ 175 (* 256 28)))) ))))
;; 123210391823

(define (u8vector->natural0 v len)
  (let lp ((n 0)
	(i (dec len)))
    (if (negative? i)
	n
	(lp (+ (u8vector-ref v i)
	       (arithmetic-shift n 8))
	    (dec i)))))

;; > (u8vector->natural0  '#u8(0 1 0 0) 4)
;; 256
;; > (u8vector->natural0  '#u8(1 0 0 0) 4)
;; 1
;; > (u8vector->natural0  '#u8(15 97 233 175 28) 5)
;; 123210391823

(define remcomm:len-len 8) ;; bytes

;; cmd is a sexpr, or actually anything
(define (remcomm:send p cmd)
  (let* ((v (object->u8vector cmd))
	 (len (u8vector-length v)))
    (write-subu8vector (natural0->u8vector len remcomm:len-len)
		       0 remcomm:len-len p)
    (write-subu8vector v 0 len p)
    (force-output p)))

;; resized as needed. XXX: ever downsize?
(define remcomm:buf-len 8) ;; follows shrinking tho
(define remcomm:buf (make-u8vector remcomm:buf-len))

;; always reuse
(define remcomm:len-len-buf (make-u8vector remcomm:len-len))

(define (read-u8vector-tmp! p len)
  ;; returned buf is only valid up to next call
  (let ((buf
	 (if (= len remcomm:len-len)
	     remcomm:len-len-buf
	     (if (< remcomm:buf-len len)
		 (let ((newbuf (make-u8vector len)))
		   (set! remcomm:buf newbuf)
		   (set! remcomm:buf-len len)
		   newbuf)
		 (begin
		   ;; u8vector->object requires the len set correctly
		   (##u8vector-shrink! remcomm:buf len)
		   ;; also have to adapt acceptable len, because the
		   ;; buf will be actually reduced in size once gc
		   ;; runs, right?
		   (set! remcomm:buf-len len)
		   remcomm:buf)))))
    (let ((rdlen (read-subu8vector buf 0 len p len)))
      (or (= rdlen len)
	  (error "only read:" rdlen))
      buf)))

(define (remcomm:recv p)
  (let* ((lenv (read-u8vector-tmp! p remcomm:len-len))
	 (len (u8vector->natural0 lenv remcomm:len-len))
	 (_
	  ;; HACK
	  (if (>= len 4428738507345651052)
		(let lp ()
		  (println (read-line p))
		  (lp))))
	 (v (read-u8vector-tmp! p len))
	 (res (u8vector->object v)))
    res))

(define (remcomm:virtual-port kind commport)
  (let* ((p (open-string (list buffering: 'line)))
	 ;; well does the buffering setting have an effect?
	 (th (make-thread
	      (lambda ()
		(let lp ()
		  ;; never eof? in-process.
		  (let ((line (read-line p)))
		    ;; or read-line ? ?
		    ;;v- XX does this need a mutex (internally)?
		    (remcomm:send commport `(port ,kind ,line)))
		  (lp))))))
    (thread-start! th)
    p))

(define (remote:start-compiler)
  (display "\ncompiler-running\n")
  (force-output)
  (let ((in (current-input-port))
	(out (current-output-port)))
    (parameterize
     ((current-output-port (remcomm:virtual-port 'output-port out))
      (current-error-port (remcomm:virtual-port 'error-port out)))
     (let lp ()
       (let ((msg (remcomm:recv in))) ;; catch exceptions?
	 (remcomm:send out
		       (remote:dispatch msg)))
       (lp)))))

(define (remote:dispatch msg)
  (with-exception-catcher
   (lambda (e)
     `(exception ,e))
   (lambda ()
     `(value ,(case (car msg)
		((ok?) 'ok)
		((ok-error)
		 (error 'ok))
		((load)
		 (let ((path (cadr msg)))
		   ;;XX or a dependency or what loading?
		   (load path)))
		((compile-expr)
		 (let ((path (cadr msg))
		       (expr (caddr msg)))
		   (compile-expr path expr)))
		((exit)
		 (exit 0))
		(else
		 (raise `(unknown-message ,(car msg)))))))))

(define-macro (local var+exprs . body)
  (let* ((var->kept_ (map (lambda (var+expr)
			    (cons (car var+expr)
				  (gensym)))
			  var+exprs))
	 (var->kept (lambda (var)
		      (cdr (assq var var->kept_)))))
    `(let ,(map (lambda (var+expr)
		  `(,(var->kept (car var+expr)) #f))
		var+exprs)
       (dynamic-wind (lambda ()
		       ,@(map (lambda (var+expr)
				(define var (car var+expr))
				(define expr (cadr var+expr))
				`(begin
				   (set! ,(var->kept var) ,var)
				   (set! ,var ,expr)))
			      var+exprs))
	   (lambda ()
	     ,@body)
	   (lambda ()
	     ,@(map (lambda (var+expr)
		      (define var (car var+expr))
		      (define expr (cadr var+expr))
		      `(begin
			 ;; simply drop value of var (and recalculate from expr)?
			 (set! ,var ,(var->kept var))))
		    var+exprs))))))

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
		  (if (> (time->seconds (file-info-last-modification-time sourceinf))
			 (time->seconds (file-info-last-modification-time (car obinf+i))))
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
