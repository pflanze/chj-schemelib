;;; Copyright 2006 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; emulate namespacing based functioncall/syntax-vs-staticstuff distinction;
;; or even just try to get some kind of namespacing working

;;(page (modified (info:now)))
;;to
;;`(page (modified ,(info:now)))


;; todo:
;; - map of namespaces which are 'constant', and if they should be stripped from the symbol or not

;(namespace "")

(require cj-env
	 ;;XXX: todo port over srfi-13
	 ;;XXX ditto? cj-expr
	 (list-util improper-map)
	 (test TEST))

(export sxmltemplates-expand
	use-sxmltemplates
	current-nonscheme-namespaces)



;; hacky:
(define current-nonscheme-namespaces (make-parameter '(html xhtml xml)))


(define (process-ns yes no)
  (lambda (sym ns-list)
    (let ((str (symbol->string sym)))
      (cond ((string-contains str ":")
	     => (lambda (pos)
		  (let ((ns (string->symbol (substring str 0 pos))))
		    (if (member ns ns-list)
			(yes str pos)
			;(string->symbol (substring str (+ pos 1) (string-length str)))
			(no sym)))))
	    (else
	     (no sym))))))

(define in-scheme-ns?
  (process-ns (lambda (str pos)
		#f)
	      (lambda (sym)
		#t)))

(define ns-turn
  (process-ns (lambda (str pos)
		(string->symbol (substring str (+ pos 1) (string-length str))))
	      (lambda (sym)
		sym)))
    
(define (ns-turn sym ns-list)
  ;; strip namespace.
  (let ((str (symbol->string sym)))
    (cond ((string-contains str ":")
	   => (lambda (pos)
		(let ((ns (string->symbol (substring str 0 pos))))
		  (if (member ns ns-list)
		      (string->symbol (substring str (+ pos 1) (string-length str)))
		      sym))))
	  (else
	   sym))))

(define (turn-symbol-expr expr ns-list)
  (expr:value-set* expr
		   (ns-turn (expr:value* expr) ns-list)))

(define (turnrest paragon-expr expr-first rest are-inside-quasiquote ns-list)
  (expr:value-set* paragon-expr
		   (cons expr-first
			 (improper-map (lambda (v)
					 (turn v are-inside-quasiquote ns-list))
				       rest))))


(define (turnattribute paragon-expr expr-first rest ns-list)
  (expr:value-set* paragon-expr
		   (cons expr-first
			 (improper-map (lambda (v)
					 ;; should be a list
					 (let ((_v (expr:value* v)))
					   (if (pair? _v)
					       ;; unquote second (and rest?(todo))
					       (expr:value-set*
						v
						(cons (car _v)
						      (let* ((v (cdr _v))
							     (_v (expr:value* v)))
							(if (pair? _v)
							    ;; ok, unquote car
							    (expr:value-set*
							     v
							     (cons (expr:value-set*
								    (car _v)
								    (list (expr:value-set*
									   (car _v)
									   'unquote)
									  (car _v))) ;; ((~analog zu make-wrapper, oder?))
								   (cdr _v)))
							    v))))
					       (error
						(string-append
						 "expected sublist in attribute in "
						 (object->string (expr:file* v))
						 "@"
						 (number->string (expr:line* v))
						 "."
						 (number->string (expr:col* v))
						 ":")
						(expr:value* v)))))
				       rest))))

;;ps  expr vs sexpr  ehr  jo  hm  nixsyntaxexpr.  lexpr  exprl?
(define (if-means-should-be-unquoted expr are-inside-quasiquote
				     yes-1ary
				     ;;no-thk  nein  muss auch arg nehmen?
				     no-1ary)
  (define xpr (expr:value* expr))
  (if (pair? xpr)
      (let* ((a* (car xpr))
	     (a (expr:value* a*)))
	(if are-inside-quasiquote
	    (if (eq? a 'unquote)
		(yes-1ary (car (expr:value* (cdr xpr))))
		(no-1ary expr))
	    (if (eq? a 'quasiquote)
		(no-1ary (cadr xpr)) ;;  eigentlich sollte es ja sogar gehen.
		(yes-1ary expr))))
      (no-1ary expr)))
;; car cdr usw   expr:cdr  usw machen ?   expr:cdr* even   könnt ma dann auch auf cdr* verkürzen


(define (turn expr are-inside-quasiquote ns-list)
  (define v (expr:value* expr))
  (cond  ((pair? v)
	  ;; depends on what the operatorposition thing is
	  (let ((expr-first (car v)))
	    (let ((a (expr:value* expr-first)))
	      (cond
	       ;; ((pair? a)  not anymore
	       ((symbol? a)
		(case a
		  ((@)
		   (if are-inside-quasiquote
		       (turnattribute expr expr-first (cdr v) ns-list)
		       expr))
		  ((include)
		   (warn "cj-sxmltemplates include")
		   (let* ((r* (cdr v))
			  (r (expr:value* r*)))
		     (if (pair? r)
			 (let ((path (expr:value* (car r))))
			   (warn "cj-sxmltemplates include path:" path)
			   (if (null? (expr:value* (cdr r)))
			       (let ((include-expr (file-read-as-expr path)))
				 (turn
				  (expr:value-set*
				   expr
				   (cons (expr:value-set*
					  expr
					  'begin)
					 (cdr include-expr)))
				  are-inside-quasiquote
				  ns-list))
			       (expr:error "too many arguments to include" expr)))
			 (expr:error "not enough arguments to include" expr))))
		  (else
		   (if (and (not (eq? a '*COMMENT*))
			    (in-scheme-ns? a ns-list))
		       (if are-inside-quasiquote
			   (expr-unquote
			    (turnrest expr expr-first (cdr v) #f ns-list))
			   (turnrest expr expr-first (cdr v) #f ns-list))
		       (if are-inside-quasiquote
			   (turnrest expr (turn-symbol-expr expr-first ns-list) (cdr v) #t ns-list)
			   (expr-quasiquote
			    (turnrest expr (turn-symbol-expr expr-first ns-list) (cdr v) #t ns-list)))))))
	       (else
		(turnrest expr expr-first (cdr v) are-inside-quasiquote ns-list))))))
	;;;; ^- sollte man bei obigen  eben  make-expr machen resp set oder sowas  also  zeilen und so info auch der gesamtliste übernehmend meinich.
	 ((symbol? v)
	  ;;(warn "alleinstehendes symbol:" v are-inside-quasiquote)
	  (if (in-scheme-ns? v ns-list)
	      (if are-inside-quasiquote
		  ;;(list 'unquote expr)
		  (expr-unquote expr)
		  expr)
	      (if are-inside-quasiquote
		  expr
		  ;; (list 'quasiquote expr)
		  (expr-quasiquote expr))))
	 ((or (##self-eval? v)
	      (null? v)) ;; '() is being used in syntax (lambda forms) !
	  expr)
	 (else
	  (warn "note: non-self-evaluating object:" v
		'|at file| (expr:file* expr)
		'|line/col| (expr:line* expr)
		(expr:col* expr))
	  ;; einfach mal stets verbatim ausgeben.  well könnte ein vector sein  dann müsst ich ihn quoten?.  wenns ne zahl ist, brauchts keins. aber shaden tuts auch nicht
	  (if are-inside-quasiquote
	      expr
	      ;; (expr:value-set* expr (list 'quasiquote expr))
	      (expr-quasiquote expr)))))


;weil compile-file segfaultet wenn man nicht ALLES mit location umgibt:

; (define (expr-unquote expr)
;   (expr:value-set* expr (list 'unquote expr)))
; (define (expr-quasiquote expr)
;   (expr:value-set* expr (list 'quasiquote expr)))

; (define (make-wrapper sym)
;   (lambda (expr)
;     ;; (expr:value-set* expr (list 'quasiquote expr))
;     (list (expr:value-set* expr sym)
; 	  expr)))
;^- segfaultet immer noch

(define (make-wrapper sym)
  (lambda (expr)
    (expr:value-set* expr (list (expr:value-set* expr sym)
				expr))))
;; ja so gehts nun.

(define expr-unquote (make-wrapper 'unquote))
(define expr-quasiquote (make-wrapper 'quasiquote))



;usage:
;(eval (turn (file-read-as-expr "testfile-xslemu.scm") #f))

(define (sxmltemplates-expand expr)
  (turn expr #f (current-nonscheme-namespaces)))

(define old-expanders #f)

(define (sxmltemplates:restore-expander!)
  (if old-expanders
      (let* ((p old-expanders)
	     (exps (car p)))
	(set! ##expand-source (car exps))
	(set! c#expand-source (cdr exps))
	(set! p (cdr p)))
      (error "no previous expander to restore")))

(define (sxmltemplates:install-expander!)
  (if (and (eq? ##expand-source sxmltemplates-expand)
	   (eq? c#expand-source sxmltemplates-expand))
      (error "sxmltemplates-expand already installed")
      (begin
	(set! old-expanders (cons (cons ##expand-source
					c#expand-source)
				  old-expanders))
	(set! ##expand-source sxmltemplates-expand)
	(set! c#expand-source sxmltemplates-expand))))

(define (use-sxmltemplates #!optional (? #t))
  ((if ? sxmltemplates:install-expander! sxmltemplates:restore-expander!)))



(TEST

> (sxmltemplates-expand '(lambda (a b) (##namespace ("" (p "so"))) (makro (html:p (boo (html:b "bold") "hah")))))
(lambda (a b)
  (##namespace ("" (p "so")))
  (makro `(p ,(boo `(b "bold") "hah"))))

)


(define expand sxmltemplates-expand) ;; for usage in chjmodule |expander| directive.

