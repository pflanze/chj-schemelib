; Thu, 14 Jul 2005 00:07:30 +0200

;;(save multiple occurences of ',vars-flat in the generated macro?)

;;((btw lang layers?))

(define-macro (let-named* ident bindings . body)
  (let* ((vars-flat (map (lambda(v)
			   (or (pair? v)
			       (error "let-named*: pair expected in bindings:" v))
			   (car v))
			 bindings))
	 (loop (gensym 'let-named*-loop-))
	 (submacro `(##define-macro
		     (,ident . args)
		     ;; build an alist  varname -> syntax
		     (let ((alis (let loop ((l '())
					    (a args))
				   (cond ((##pair? a)
					  (or (##pair? (##cdr a))
					      (gambit-interpreter-env#error "let-named* call: identifyer must be called with an even number of arguments: " ',ident args))
					  (let ((key (##car a))
						(val (##cadr a)))
					    (or (##keyword? key)
						(gambit-interpreter-env#error "let-named* call: expecting keyword in identifyer call: " ',ident key))
					    (let ((varnam (##string->symbol
							   (##keyword->string key))))
					      (or (##member varnam ',vars-flat)
						  (gambit-interpreter-env#error "let-named* call: no such variable:" ',ident varnam))
					      (and (##assoc varnam l)
						   (gambit-interpreter-env#error "let-named* call: same keyword given twice: " ',ident varnam))
					      (loop (##cons (##cons varnam val) l)
						    (##cddr a)))))
					 (else l)))))
		       (##cons ',loop
			       (##map (lambda(v)
					(cond ((##assoc v alis)
					       => ##cdr)
					      (else v))) ',vars-flat))))))
    `(let ,loop ,bindings
	  ,submacro
	  ,@body)))

;;(Note: non-namespacesafe identifyiers: quasiquote, and the usual syntax like cond let etc.)

;simple test:
; > (let-named* fun ((a 3)(b 4)) (println "a:" a ", b:" b) (if (> b 0) (fun b: (- b 1)) a))
; a:3, b:4
; a:3, b:3
; a:3, b:2
; a:3, b:1
; a:3, b:0
; 3
; > (let-named* fun ((a 3)(b 4)) (println "a:" a ", b:" b) (if (> b 0) (fun b: (- b 1) a: (modulo b 2)) a))
; a:3, b:4
; a:0, b:3
; a:1, b:2
; a:0, b:1
; a:1, b:0
; 1
