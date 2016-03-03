
;; part of the `fail` module that can be implemented without `dot-oo`,
;; to satisfy `cj-typed`'s dependency

(require cj-struct
	 (cj-functional compose)
	 (cj-env symbol-value-or))

(export failure?
	failure-string)

(define-struct failure
  stack)

(define (failure-show v)
  ;; XX but now need to rely on dot-oo or cycle workaround hack
  ;; *anyway*. Bah. TODO: avoid relying on cj-typed from dot-oo ? Or
  ;; split cj-typed ?
  (let ((.show (symbol-value-or
		'.show
		(lambda ()
		  (lambda (v)
		    ;;  in hack to workaround dependency cycle,
		    (error ".show not yet defined"))))))
    (map .show
	 (failure-stack v))))

(define failure-string
  (compose object->string failure-show))
