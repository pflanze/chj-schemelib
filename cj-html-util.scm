(require)

;; for html represented as sxml

(define (maillink email)
  `(a (@ (href ,(string-append "mailto:"
			       email)))
      ,email))
