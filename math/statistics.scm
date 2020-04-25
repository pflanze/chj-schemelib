;;; Copyright 2019-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         seq
         (cj-math list-average stream-average
                  list-standard-deviation list-standard-deviation-from
                  list-variance list-variance-from)
         test
         oo-lib-vector
         oo-lib-u32vector)

(export (methods seq.mean
                 seq.product
                 seq.geomean
                 seq.harmmean)
        (generics .std
                  .stdm
                  .var
                  .varm)
        (classes mean&std
                 mean&var)
        ;; not all seq supported yet:
        (methods seq.mean&std
                 seq.mean&var))


"Basic statistics functions."

;; Partially inspired by or translated from the Julia library
;; StatsBase.jl (which is licensed under the MIT License)
;; https://github.com/JuliaStats/StatsBase.jl


(include "../cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


(def. (seq.mean s)
  (/ (.fold s + 0)
     (.length s)))

(def. ilist.mean list-average)
(def. istream.mean stream-average)



(def. seq.geomean.1
  (=>* (.map log)
       .mean
       exp))

(def. seq.product
     (=>* (.fold * 1)))

(TEST
 > (def (t v)
        (list (.product v)
              (.product (.vector v))
              (.product (.u32vector v))))
 > (t '())
 (1 1 1)
 > (t '(2))
 (2 2 2)
 > (t '(2 3))
 (6 6 6))


(def. (seq.geomean.2 s)
  (expt (.product s) (/ (.length s))))

(TEST
 > (def (t l)
        (map (lambda (f) (f l))
             (list .geomean.1
                   .geomean.2)))
 > (t '(2 8))
 (4.
  4)
 > (t '(4 1 1/32))
 (.49999999999999994
  1/2)
 > (t '(4. 1 1/32))
 (.49999999999999994
  .5)
 > (t '(4. 1. .03125))
 (.49999999999999994
  .5)
 > (t (list (sqrt -2)))
 (8.659560562354934e-17+1.4142135623730951i
  +1.4142135623730951i)
 > (t (list (sqrt -2) (sqrt 2)))
 (1.0000000000000002+1.i
  1.0000000000000002+1.i)
 )


;; the better one for now?
(def. seq.geomean seq.geomean.2)



;; harmmean(a) = inv(mean(inv, a))

(def. seq.harmmean
  ;; "the harmonic mean of a collection"
  (=>* (.map /)
       .mean
       /))



(def. (ilist.std s #!key corrected?)
  (list-standard-deviation s whole?: (not corrected?)))

(def. (ilist.stdm s m #!key corrected?)
  (list-standard-deviation-from m (not corrected?) s))


(def. (ilist.var s #!key corrected?)
  (list-variance s whole?: (not corrected?)))

(def. (ilist.varm s m #!key corrected?)
  (list-variance-from m (not corrected?) s))


(defclass (mean&std mean std))

(def. (seq.mean&std s #!key corrected?)
  (let (m (.mean s))
    (mean&std m
              (.stdm s m corrected?: corrected?))))


(defclass (mean&var mean std))

(def. (seq.mean&var s #!key corrected?)
  (let (m (.mean s))
    (mean&var m
              (.varm s m corrected?: corrected?))))



(TEST
 > (def vs '(727.7 1086.5 1091. 1361.3 1490.5 1956.1))
 > (.mean&std vs)
 [(mean&std) 1285.5166666666667 384.2844190469114]
 > (.mean&std vs corrected?: #t)
 [(mean&std) 1285.5166666666667 420.96248961952256]

 > (def grades '(2 4 4 4 5 5 7 9))
 > (.mean&var grades corrected?: #t)
 [(mean&var) 5 32/7]
 > (.mean&var grades)
 [(mean&var) 5 4]
 > (.mean&std grades)
 [(mean&std) 5 2]
 )
