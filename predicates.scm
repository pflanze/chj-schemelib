
(define false? not) ;; so as to be able to use "false." as OO prefix
(define (anything? x) #t)
;; maybe also, since at it:
(define (true? x) (eq? x #t)) ;; dangerous to mistake?
(define (true x)
  ;; any kind of true; identity
  (not (not x)))

