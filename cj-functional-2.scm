
(require cj-match
	 test
	 (cj-env-1 inc))

(export chain
	chain*)

;; The "Clojure-macro". What was it named? XXX

(define (chain-expand start exprs)
  (let next ((exprs exprs)
	     (res start))
    (if (null? exprs)
	res
	(next (cdr exprs)
	      (mcase (car exprs)
		     (`(`call . `rest)
		      `(,call ,res ,@rest)))))))

(TEST
 > (chain-expand 'input '((foo-set 1) (bar-set 2)))
 (bar-set (foo-set input 1) 2))

(define-macro* (chain start . exprs)
  (chain-expand start exprs))

(define-macro* (chain* . exprs)
  (with-gensym
   V
   `(lambda (,V)
      ,(chain-expand V exprs))))

(TEST
 > ((chain* (inc)) 10)
 11)
