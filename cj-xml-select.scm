
(require (list-util xone))

(define (body x)
  (cdr x)) ;; ignoring attributes krck

(define (tag-name tn)
  (car tn))

(define (tag? v)
  (and (pair? v)
       (symbol? (car v))))

(define (is-tag tn)
  (lambda (t)
    (and (tag? t)
	 (eq? (tag-name t) tn))))

(define (xsubtag t tn)
  (xone (filter (is-tag tn) (body t))))


