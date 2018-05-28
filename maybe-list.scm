(require easy)

(include "cj-standarddeclares.scm")


(def (list-maybe-ref lis [natural0? n])
     (cond
      ((null? lis) #f)
      ((zero? n) (first lis))
      (else (list-maybe-ref (rest lis) (- n 1)))))


(def (maybe-fifth lis)
     (list-maybe-ref lis 4))


