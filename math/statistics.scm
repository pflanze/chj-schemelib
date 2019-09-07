;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         seq
         (cj-math list-average stream-average)
         test)

(export (method .mean)
        (method .product)
        (method .geomean)
        )

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

