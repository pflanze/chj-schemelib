;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         Result
         monad/syntax
         monad/lib)

;; (export )
;; the ones that the template defines; how to pick those up from there?


(include "../cj-standarddeclares.scm")

(def-monad-lib-for Result)


(TEST
 > (def (Result:/ a b)
        (if (zero? b)
            (Error 'division-by-zero)
            (Ok (/ a b))))
 > (Result:mapM (C Result:/ 1 _) '(1 2 3))
 [(Ok) (1 1/2 1/3)]
 > (Result:mapM (C Result:/ 1 _) '(1 0 2 3))
 [(Error) division-by-zero])
