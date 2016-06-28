
(require cj-match
	 test
	 (cj-env-1 inc))

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
	(next (cdr exprs)
	      (mcase (car exprs)
		     (`(`call . `rest)
		      `(,call ,res ,@rest)))))))

(TEST
 > (=>-expand 'input '((foo-set 1) (bar-set 2)))
 (bar-set (foo-set input 1) 2))

(define-macro* (=> start . exprs)
  (=>-expand start exprs))

(define-macro* (=>* . exprs)
  (with-gensym
   V
   `(lambda (,V)
      ,(=>-expand V exprs))))

(TEST
 > ((=>* (inc)) 10)
 11)


;; bah, copy-paste except for the last line
(define (=>>-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(next (cdr exprs)
	      (mcase (car exprs)
		     (`(`call . `rest)
		      `(,call ,@rest ,res)))))))


;; dito
(define-macro* (=>> start . exprs)
  (=>>-expand start exprs))

(define-macro* (=>>* . exprs)
  (with-gensym
   V
   `(lambda (,V)
      ,(=>>-expand V exprs))))


