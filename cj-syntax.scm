

;; Random syntax extensions? (Utilities.)


(require cj-match
	 define-macro-star)


;; Like part of mcase, but simpler and not doing the overhead of
;; calling source-code: 'predicate case'
(define-macro* (pcase expr . cases)
  (with-gensym
   V
   `(let ((,V ,expr))
      (cond ,@(map
	       ;; heh now falling back to mcase interesting..
	       (mcase-lambda
		(`(`pred `body)
		 `((,pred ,V)
		   ,body)))
	       cases)
	    ;; ah yep and also don't permit nonmatches; XX hm, check
	    ;; whether the last case is 'else'?
	    (else
	     (pcase-error ,V))))))

(define (pcase-error val)
  (error "no match for:" val))

(TEST
 > (pcase "foo" (string? 'yes))
 yes
 > (%try-error (pcase 'foo (string? 'yes)))
 #(error "no match for:" foo)
 )

