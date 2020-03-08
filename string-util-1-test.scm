;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
         easy
         string-util-1
         (char-util char-one-of?/))

;; Just one more test with "abcd" as the string, easier to grok for my
;; head. Separate file for using char-one-of?/ (meh, want test
;; dependency stuff?..).

(TEST
 > (def s "abcd")
 > (define t string-split)
 > (t s (char-one-of?/ "bc"))
 ("a" "" "d")
 > (t s (char-one-of?/ "bc") 'right)
 ("a" "b" "cd")
 > (t s (char-one-of?/ "bc") 'left)
 ("ab" "c" "d")
 )

(TEST
 > (def s (source "abcd" (location '(f) (position 10 13))))
 > (define (t . args)
     (let ((ss (apply string-split/location args)))
       (list (map source-code ss)
             (map (lambda (v) (location-string (source-location v))) ss))))
 > (t s (char-one-of?/ "bc"))
 (("a" "" "d")
  ("(f)@10.13" "(f)@10.15" "(f)@10.16"))
 > (t s (char-one-of?/ "bc") 'right)
 (("a" "b" "cd")
  ("(f)@10.13" "(f)@10.15" "(f)@10.16"))
 > (t s (char-one-of?/ "bc") 'left)
 (("ab" "c" "d")
  ("(f)@10.13" "(f)@10.15" "(f)@10.16"))
 )
