(require define-macro-star)

(define-macro* (if-debug . body)
  (if #f
      `(begin ,@body)
      '(begin)))
