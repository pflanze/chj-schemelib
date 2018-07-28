
(require define-macro-star)

(export (macro @vector-ref)
	(macro @vector-set!)
	(macro @vector-length))

(include "cj-standarddeclares.scm")

(define-macro* (@vector-ref v i)
  ;; need unsafe mode for ##vector-ref to be compiled efficiently!
  `(##let ()
	  (declare (not safe))
	  (##vector-ref ,v ,i)))

(define-macro* (@vector-set! v i val)
  ;; need unsafe mode here, too?
  `(##let ()
	  (declare (not safe))
	  (##vector-set! ,v ,i ,val)))

(define-macro* (@vector-length v)
  ;; need unsafe mode here, too?
  `(##let ()
	  (declare (not safe))
	  (##vector-length ,v)))

