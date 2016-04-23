
'(require define-macro-star)

(define-macro* (require . body)
  `(begin))

(define-macro* (export . forms)
  '(begin))

