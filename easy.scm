;; short identifiers

(define-macro* (& . args)
  ;; `(thunk ,@args)
  `(lambda () ,@args))

(define-macro* (defstruct . args)
  `(define-struct. ,@args))

(define-macro* (def first . rest)
  (if (pair? (source-code first))
      `(define-typed ,first ,@rest)
      `(define ,first ,@rest)))

(define-macro* (def. . args)
  `(define. ,@args))

(define-macro* (defenum name . args)
  `(define-enum ,name ,@args))

(define-macro* (defmacro . args)
  `(define-macro* ,@args))

(define-macro* (defvalues . args)
  `(define-values ,@args))


(def comp compose)
(defmacro (comp* . args)
  `(compose* ,@args))

