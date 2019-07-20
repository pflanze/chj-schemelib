;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XX merge with/rename to/from memoize.scm

(require define-macro-star

         ;; (cj-source-util schemedefinition-arity:pattern->template)
         ;; is included in define-macro-star.scm

         ;; cj-typed -- actually
         ;; schemedefinition-arity:pattern->template works directly
         ;; with typed arguments, too!

         (cj-env-2 xcase)
         (vector-util let-vector)
         (list-util let-pair)
         (simple-match-1 assert*)
         (cj-env symbol-append))

(export cached-0
        cached-1
        cached-2
        cached-*
        (macro cached-lambda)
        (macro def-cached))


;; Name those cached-.., not cached/.., only use .../$n for arity of
;; the function itself (like Erlang), OK?

(define (cached-0 fn)
  (let* ((t (box #f))
         ;; heh: can re-use t itself as nothing value!
	 (nothing t))
    (set-box! t t)
    (lambda (v)
      (let ((r (unbox t)))
	(if (eq? r nothing)
	    (let ((r (fn v)))
	      (table-set! t v r)
	      r)
	    r)))))

(define (cached-1 fn)
  (let* ((t (make-table))
	 ;; heh: can re-use t itself as nothing value!
	 (nothing t))
    (lambda (v)
      (let ((r (table-ref t v nothing)))
	(if (eq? r nothing)
	    (let ((r (fn v)))
	      (table-set! t v r)
	      r)
	    r)))))

(define (cached-2 fn)
  (let* ((t (make-table))
	 ;; heh: can re-use t itself as nothing value!
	 (nothing t))
    (lambda (v1 v2)
      (let* ((vs (cons v1 v2))
             (r (table-ref t vs nothing)))
	(if (eq? r nothing)
	    (let ((r (fn v1 v2)))
	      (table-set! t vs r)
	      r)
	    r)))))

(define cached:max-arity-fixed 2)


(define (cached-* fn)
  (let* ((t (make-table))
	 ;; heh: can re-use t itself as nothing value!
	 (nothing t))
    (lambda vs
      (let ((r (table-ref t vs nothing)))
	(if (eq? r nothing)
	    (let ((r (apply fn vs)))
	      (table-set! t vs r)
	      r)
	    r)))))



(define-macro* (cached-lambda binds . body)
  (let-vector
   ((qualifier arity) (schemedefinition-arity:pattern->template
                       ;; source-code is essential here; change ^ ?
                       (source-code binds)))

   (define (fallback)
     `(cached-* (lambda ,binds ,@body)))
       
   (xcase qualifier
          ((exact)
           (cond ((<= arity cached:max-arity-fixed )
                  `(,(symbol-append "cached-" (number->string arity))
                    (lambda ,binds ,@body)))
                 (else
                  (fallback))))
          ((at-least up-to)
           (fallback)))))


;; Use 'easy' naming style but don't depend on it for definition for
;; less bootstrapping pressure (just in case I'd use it in the
;; system). Thanks to |lambda| being redefined by easy, this works with
;; typed automatically, too (assuming easy is actually loaded; anyway,
;; HACK).

(define-macro* (def-cached name+binds . body)
  (assert*
   pair? name+binds
   (lambda (name+binds)
     (let-pair
      ((name binds) name+binds)
      `(define ,name (cached-lambda ,binds ,@body))))))

