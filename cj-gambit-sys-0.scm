
(require define-macro-star)

(export (macro @vector-ref)
	(macro @vector-set!)
	(macro @vector-length))

(include "cj-standarddeclares.scm")

(define-macro* (@vector-ref v i)
  ;; need unsafe mode for ##vector-ref to be compiled efficiently!
  (let ((V (gensym 'V))
	(I (gensym 'I)))
    `(##let ((,V ,v)
	     (,I ,i))
	    (##declare (not safe))
	    (##vector-ref ,V ,I))))

(define-macro* (@vector-set! v i val)
  ;; need unsafe mode here, too?
  (let ((V (gensym 'V))
	(I (gensym 'I))
	(VAL (gensym 'VAL)))
    `(##let ((,V ,v)
	     (,I ,i)
	     (,VAL ,val))
	    (##declare (not safe))
	    (##vector-set! ,V ,I ,VAL))))

(define-macro* (@vector-length v)
  ;; need unsafe mode here, too?
  (let ((V (gensym 'V)))
    `(##let ((,V ,v))
	    (##declare (not safe))
	    (##vector-length ,V))))

