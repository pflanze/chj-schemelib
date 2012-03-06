;; A monad[?] with convention to return v and the monadic value m (no CPS)

;; ((mfn v) m) -> (cons v* m*)
;; (mfn v) -> m -> (cons v* m*)
;; (define (m:map mfn l)
;;   (lambda (m)
;;     ))

(define vals cons)
(define val0 car)
(define val1 cdr)

(define v+m vals)
(define val-v val0)
(define val-m val1)

(define (m:map mfn l m)
  (let rec ((l l)
	    (m m))
    (delay
      (if (null? l)
	  (v+m l m)
	  (let* ((v (force (mfn (car l) m)))
		 (w (force (rec (cdr l)
				(val-m v)))))
	    (v+m (cons (val-v v)
		       (val-v w))
		 (val-m w)))))))

;; (define (m:inc v m)
;;   (v+m (cons (inc v) m)
;;        (* m 2)))
;; > (m:map m:inc '(1 2 3 4) 10)
;; (((2 . 10) (3 . 20) (4 . 40) (5 . 80)) . 160)

