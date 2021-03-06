;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (dot-oo resolve.))

(export (method number-vector.smoothfn))

"Like number-vector.mapfn / iseq-of-number.mapfn but smoothed"
;; in future also add alist.smoothfn well however exact you want to
;; name the type. Add type to math/predicates.


(include "../cj-standarddeclares.scm")


(def. (number-vector.smoothfn vec #!optional dist->weight)
  "Function over given vector that resolves 0..1 across the vector,
and uses weighted average using `(/ (dist->weight
dist))`. dist->weight is (comp square square) by default."
  ;; When too close, just take the value.

  (let ()
   ;; ! corresponding to = in C (or to ! in Erlang, somewhat :)
   (define-macro (! var v)
     `(f64vector-set! ,var 0 ,v))
   (define-macro (ref var)
     `(f64vector-ref ,var 0))
   (define-macro (+! var v)
     `(! ,var (fl+ (ref ,var) ,v)))
   (define-macro (square v)
     `(fl* ,v ,v))
   (define-macro (decl v)
     `(def ,v (f64vector 0.)))
  
   (let* ((len (.length vec))
          (ref-inexact (resolve. .ref-inexact vec))

          ;; Conversion to i's scale
          (x-scaler (exact->inexact (dec len))))

     (decl tot)
     (decl tot-weight)
     (decl dist)
     (decl dist*)
     (decl weight)
      
     (lambda ([flonum? x])
       ;; x \in 0..1
       ;; Zoom out a bit so that the edge values can be seen fully:
       (declare (not safe))
       (let* ((x-scaled (fl* x x-scaler)))
         (! tot 0.)
         (! tot-weight 0.)
         (for..< (i 0 len)
                 (! dist (fl- x-scaled (exact->inexact i)))
                 (if dist->weight
                     (! dist* (-> flonum? (dist->weight (ref dist))))
                     ;; Interesting, (abs dist) is spiky, (square
                     ;; dist) is nice but over-reacting, abs ^3 and
                     ;; ^4 are 'rounded-blocky' as is probably best
                     ;; for this.
                     (! dist* (square (square (ref dist)))))
                 (! weight (fl/ (ref dist*)))
                 ;; (when (> (ref weight) 1000000.)
                 ;;       (warn "weight=" (ref weight)))
                 (+! tot-weight (ref weight))
                 (+! tot (fl* (ref weight)
                              (ref-inexact vec i))))
         (let (w (ref tot-weight))
           (if (or (infinite? w) (nan? w) (fl> w 1e7)) ;; ?
               ;; too close, just take the value.
               (ref-inexact vec (integer (fl+ x-scaled 0.5)))
               (fl/ (ref tot) w))))))))

(TEST
 > (def h '#u32(5 1 2 6 3 1))
 > (def f (.smoothfn h))
 > (f 0.)
 5.
 > (f (/ 1. 5))
 1.
 > (f (/ 2. 5))
 2.
 > (f (/ 4. 5))
 3.
 > (f (/ 5. 5))
 1.
 > (f 0.5)
 3.974070249946426)

