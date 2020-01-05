;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy ;; since Maybe needs it too, anyway
         srfi-1
         ;; optim-values
         Maybe
         test)

(export Maybe-split-at-reverse
        Maybe-split-at)

"Variants of SRFI-1 functions that return Maybe instead of throwing
exceptions when given too short lists"


(define-macro (check-arg . body)
  '(void))

(def (Maybe-split-at-reverse x k) -> (Maybe (values-of ilist?
                                                       ilist?))
     (check-arg integer? k split-at)
     (let lp ((lis x) (k k) (rfront '()))
       (if (zero? k)
           (Just (values rfront lis))
           (if-let-pair ((a r) lis)
                        (lp r (dec k) (cons a rfront))
                        (Nothing)))))

(def (Maybe-split-at x k) -> (Maybe (values-of ilist?
                                               ilist?))
     (>>= (Maybe-split-at-reverse x k)
          (lambda-values ((rfront lis))
                    (return (values (reverse rfront)
                                    lis)))))


(TEST
 > (.show (Maybe-split-at-reverse '(5 6 7) 2))
 (Just (values (list 6 5) (list 7)))
 > (.show (Maybe-split-at '(5 6 7) 2))
 (Just (values (list 5 6) (list 7)))
 > (.show (Maybe-split-at '(5 6 7) 0))
 (Just (values (list) (list 5 6 7)))
 > (.show (Maybe-split-at '(5 6 7) 3))
 (Just (values (list 5 6 7) (list)))
 > (.show (Maybe-split-at '(5 6 7) 4))
 (Nothing))

