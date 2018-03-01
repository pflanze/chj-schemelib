
(require define-macro-star
	 (cj-source source-error)
	 (list-util let-pair)
	 ;; for tests:
	 test
	 (cj-env-1 inc)
	 srfi-1)

(export (macro =>)
	(macro =>*)
	(macro =>>)
	(macro =>>*))

;; The "Clojure-macros". Clojure calls them -> and ->> instead, but ->
;; is taken by cj-typed. Called this "chain" previously (but then how
;; to call the other variant, chain> ?)

(define (=>-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(let-pair ((expr exprs*) exprs)
		  (next exprs*
			(let ((expr* (source-code expr)))
			  (cond
			   ((pair? expr*)
			    `(,(car expr*) ,res ,@(cdr expr*)))
			   ((symbol? expr*)
			    `(,expr ,res))
			   (else
			    (source-error
			     expr
			     "expecting a form or a symbol")))))))))

(TEST
 > (=>-expand 'input '((foo-set 1) (bar-set 2)))
 (bar-set (foo-set input 1) 2))

(define-macro* (=> start . exprs)
  (=>-expand start exprs))

(define-macro* (=>* expr0 . exprs)
  (with-gensym
   V
   (if (symbol? (source-code expr0))
       `(##lambda ,V
	     ,(=>-expand `(##apply ,expr0 ,V) exprs))
       ;; otherwise can't support multiple values:
       `(##lambda (,V)
	     ,(=>-expand V (cons expr0 exprs))))))

(TEST
 > ((=>* (inc)) 10)
 11
 > ((=>* inc inc) 10)
 12
 ;; multiple arguments:
 > ((=>* + inc))
 1
 > ((=>* + inc) 2 3)
 6
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (& ((=>* (+) inc) 2 3)))
 #t
 > ((=>* (+) inc) 2)
 3)


;; bah, copy-paste except for one line
(define (=>>-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(let-pair ((expr exprs*) exprs)
		  (next exprs*
			(let ((expr* (source-code expr)))
			  (cond
			   ((pair? expr*)
			    ;; only change here:
			    `(,(car expr*) ,@(cdr expr*) ,res))
			   ((symbol? expr*)
			    `(,expr ,res))
			   (else
			    (source-error
			     expr
			     "expecting a form or a symbol")))))))))


;; dito
(define-macro* (=>> start . exprs)
  (=>>-expand start exprs))

(define-macro* (=>>* expr0 . exprs)
  (with-gensym
   V
   (if (symbol? (source-code expr0))
       `(##lambda ,V
	     ,(=>>-expand `(##apply ,expr0 ,V) exprs))
       ;; otherwise can't support multiple values:
       `(##lambda (,V)
	     ,(=>>-expand V (cons expr0 exprs))))))

;; it's actually REALLY all copy-paste except for =>>-expand call,
;; which is a function, bah.todo.


(TEST
 > (=> (=>> (iota 10)
	    (map inc)
	    (filter even?))
       (take 2))
 (2 4)
 > ((=>>* (inc)) 10)
 11
 > ((=>>* inc (inc) inc inc) 10)
 14
 ;; multiple arguments:
 > (with-exception-catcher divide-by-zero-exception?
			   (& ((=>>* + (/ 2)))))
 #t
 > ((=>>* + (/ 2)) 2 3)
 2/5
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (& ((=>>* (+) inc) 2 3)))
 #t
 > ((=>>* (+) inc) 2)
 3)

