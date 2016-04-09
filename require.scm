;; 'parse them', sort them, kr(ypton)

(require easy)


(def (require-decl.modulename v)
     (mcase v
	    (symbol?
	     (source-code v))
	    (pair?
	     (source-code (car (source-code v))))))

;; module-basename (vs. full module name in the future?)
(def (path-string.modulename p) -> symbol?
     (string.symbol (basename p ".scm")))


(def modules-without-require-forms
     '(;; cj-source
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

(def (path-string.topo-relation p)
     (let ((form (call-with-input-file p read))
	   (mname (path-string.modulename p)))
       ((named rec
	       (mcase-lambda
		(`(require . `rest)
		 (topo-relation mname
				(map require-decl.modulename rest)))
		(`(quote `q)
		 (rec q))
		(else
		 (if (memq mname modules-without-require-forms)
		     (topo-relation mname '())
		     (error "file does not have a require form as its first form:"
			    p)))))
	form)))

;; tests see in require-util.scm
