;; Unlike _-alist-ref doen't hard-code the pairings using pairs,
;; instead use parametrized accessors

;; Perhaps call it associative-list instead?

(require easy
	 Maybe)

(defmodule (<list-ref> key? .key .equal?)

  (export Maybe-ref)

  (def (Maybe-ref lis #(key? key))
       (let lp ((l lis))
	 (cond ((pair? l)
		(let-pair ((a r) l)
			  (if (.equal? key (.key a))
			      (Just  a)
			      (lp r))))
	       ((null? l)
		(Nothing))
	       (else
		(error "improper list:" alis))))))

