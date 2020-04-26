;;; Copyright 2019-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export (methods pairs.xs
                 pairs.ys
                 pairs.x2s
                 pairs.xys
                 pairs.least-squares
                 pairs.fit)
        #!optional
        pairs?)

"Linear regression (least squares)"

(include "../cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


;; Same as |alist?| but only partially testing:
(def pairs? (iseq-of pair?))

(def. pairs.xs (=>* (.map car)))
(def. pairs.ys (=>* (.map cdr)))
(def. pairs.x2s (=>* (.map (=>* car square))))
(def. pairs.xys (=>* (.map (lambda-pair ((x y)) (* x y)))))

(def. (pairs.least-squares ps) -> (values-of real? real?)
  "For ((x . y)...) Returns (values b m) where f(x)= b + m*x"
  ;; list for higher 'orders' ?
  (let ((sx (=> ps .xs .sum))
        (sy (=> ps .ys .sum))
        (sx2 (=> ps .x2s .sum))
        (sxy (=> ps .xys .sum))
        (n (.length ps)))
    (let* ((m (/ (- (* n sxy) (* sx sy))
                 (- (* n sx2) (square sx))))
           (b (/ (- sy (* m sx))
                 n)))
      (values b m))))

(def. (pairs.fit ps)
  (letv ((b m) (.least-squares ps))
        (lambda (x)
          (+ b (* m x)))))

(TEST
 > (def s1 '((5 . 1) (10 . 3)))
 > (.xs s1)
 (5 10)
 > (.ys s1)
 (1 3)
 > (.x2s s1)
 (25 100)
 > (.xys s1)
 (5 30)
 > (.list (.least-squares s1))
 (-1 2/5)
 > (def f (.fit s1))
 > (f 5)
 1
 > (f 10)
 3)

(TEST
 > (def s1 '((5 . 1) (10 . 3) (10 . 4)))
 > (.xys s1)
 (5 30 40)
 > (.list (.least-squares s1))
 (-3/2 1/2)
 > (def f (.fit s1))
 > (f 5)
 1
 > (f 10)
 7/2
 > (f 10.)
 3.5)

(TEST
 > (def s1 '((5 . 1) (10 . 3) (11 . 4)))
 > (.xys s1)
 (5 30 44)
 > (.list (.least-squares s1))
 (-43/31 29/62)
 > (def f (.fit s1))
 > (f 5)
 59/62
 > (f 10)
 102/31
 > (f 10.)
 3.2903225806451606
 > (f 11)
 233/62
 > (f 11.)
 3.7580645161290316)

