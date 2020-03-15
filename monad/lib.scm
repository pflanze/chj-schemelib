;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         monad/syntax)

(export (template def-monad-lib-for))


(include "../cj-standarddeclares.scm")


(deftemplate (def-monad-lib-for <M>)

  ;; Monad M => (a -> M b) -> [a] -> M [b]
  (def (<M>:mapM fn l) -> <M>?
       "Also called `traverse` in Haskell. Only for lists though.
Eager since it tells right away wether it will succeed"
       (let lp ((l l)
                (res '()))
         (if-let-pair ((a l*) l)
                      (>>= (fn a)
                           (lambda (a*)
                             (lp l*
                                 (cons a* res))))
                      (return (reverse res))))))


