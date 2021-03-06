;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles-transformations
         math/visualize/plot
         test)

(export (methods wbtable.interpolate-plot-function
                 subtitles-directives.interpolate-plot))

(include "cj-standarddeclares.scm")

"For examination/debugging/development of subtitles-transformations"



(def. (wbtable.interpolate-plot-function pts)
  (let (f0 (.interpolate-function-for pts))
    (lambda ([real? t]) -> real?
       (Maybe:if-let ((f1 (f0 t)))
                     (- (f1 t) t)
                     ;; AH don't have location info here
                     (error "can't find" t)))))

(defparameter current-interpolate-plot-stepsize 1000)

(def. (subtitles-directives.interpolate-plot l)
  "Show plot of new vs. original t values:

  (=> v bare->subtitle-items .interpolate-plot)"
  ;; and should show dots, too. *Meh*

  (let (tbl (.shift-points-wbtable l))
    (-> Maybe?
        (mlet ((t0 (.Maybe-min tbl))
               (tend (.Maybe-max tbl)))
              (let ((x0 (car t0))
                    (x1 (car tend)))
                (warn "x0,x1=" x0 x1) ;; should `plot` show it?
                (let* ((nonT (.filter l (complement T?)))
                       (tims (.filter l subtitles-time?)))
                  (warn "number of nonT items:" (.length nonT))
                  (warn "number of time items:" (.length tims)))
                (let (stepfunction
                      ;; HACK: plot that steps up by a constant for
                      ;; every time directive to see where they are.
                      (let (steptbl
                            (=> (.list tbl)
                                (.map/iota (lambda-pair
                                            ((tx ty) i)
                                            (cons tx (* i (current-interpolate-plot-stepsize)))))
                                list->real/real-wbtable))
                        (lambda (t)
                          ;; sharp steps, actually what I want
                          (Maybe:cond ((.Maybe-ref steptbl t)
                                       (cdr it))
                                      ((.Maybe-next steptbl t)
                                       (cdr it))
                                      ((.Maybe-prev steptbl t)
                                       ;; last one ?
                                       (WARN "missing entry")
                                       (cdr it))
                                      (else
                                       (error "bug"))))))
                  (return (plot (list (.interpolate-plot-function tbl)
                                      stepfunction)
                                x0 x1))))))))

