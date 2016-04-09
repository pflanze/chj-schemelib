;; 'parse them', sort them, kr(ypton)

(require (cj-io-util basename))


(define (require-decl.modulename v)
  (cond ((symbol? v)
	 (source-code v))
	((pair? v)
	 (source-code (car (source-code v))))
	(else
	 (error "no match:" v))))

;; module-basename (vs. full module name in the future?)
(define (path-string.modulename p) ;; -> symbol?
  (string->symbol (basename p ".scm")))


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
