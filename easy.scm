;; short identifiers

(require define-macro-star
	 cj-env ;; identity ?, define-if-not-defined
	 cj-functional ;; compose
	 cj-struct
	 cj-typed
	 dot-oo ;; incl. define.
	 more-oo ;; well, just re-export?
	 srfi-11
	 define-module)

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

(define-macro* (defparameter . args)
  `(define-parameter ,@args))

(define-macro* (def-once . args)
  `(define-if-not-defined ,@args))

(def comp compose)
(defmacro (comp* . args)
  `(compose* ,@args))

(def id identity)

(defmacro (defmodule . args)
  `(define-module ,@args))

