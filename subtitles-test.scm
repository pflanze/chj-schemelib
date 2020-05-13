;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles
         test)

(export)

(include "cj-standarddeclares.scm")

"Tests for subtitles.scm"


(TEST ;; subtitles-time
 > (milliseconds->tim 1)
 [(tim) 0 0 0 1]
 > (milliseconds->tim 999)
 [(tim) 0 0 0 999]
 > (milliseconds->tim 1000)
 [(tim) 0 0 1 0]
 > (def ms (+ (* 63 1000) 7))
 > ms
 63007
 > (milliseconds->tim ms)
 [(tim) 0 1 3 7]
 > (.milliseconds #)
 63007
 > (tim 1 30 7 500)
 [(tim) 1 30 7 500]
 > (.milliseconds #)
 5407500
 > (milliseconds->tim #)
 [(tim) 1 30 7 500]
 > (.tim (.- (tim 1 10 5 0) (tim 1 10 3 0)))
 [(tim) 0 0 2 0]
 > (.tim (.- (tim 1 10 5 0) (tim 1 10 3 100)))
 [(tim) 0 0 1 900]
 > (.+ (tim 1 10 5 0) 10)
 [(tim) 1 10 5 10]
 > (.+ (tim 1 10 5 0) 1000)
 [(tim) 1 10 6 0]
 > (.+ (tim 1 10 5 0) -1000)
 [(tim) 1 10 4 0]

 > (.< (tim 0 1 8 862) (tim 0 1 8 862))
 #f
 > (.< (tim 0 1 8 861) (tim 0 1 8 862))
 #t
 > (.< (tim 0 1 6 862) (tim 0 1 8 862))
 #t
 > (.> (tim 0 1 8 862) (tim 0 1 8 862))
 #f
 > (.= (tim 0 1 8 862) (tim 0 1 8 862))
 #t
 > (.>= (tim 0 1 8 862) (tim 0 1 8 862))
 #t)

(TEST ;; subtitles-time
 > (def old-current-tm-delay (current-tm-delay))
 > (current-tm-delay -100)
 > (tm 60.53)
 [(tm) 60.53]
 > (.milliseconds #)
 60430
 > (.- (tm 60.53) (tm 1))
 [(subtitles-milliseconds) 59530]
 > (.- (tm 60.53) (tm 1.00001))
 [(subtitles-milliseconds) 59530]
 ;; conversions
 > (.tm (tm 334.9))
 [(tm) 334.9]
 > (.tim (tm 334.9))
 [(tim) 0 5 34 800]
 > (.subtitles-milliseconds (tm 334.9))
 [(subtitles-milliseconds) 334800]
 > (.tim #)
 [(tim) 0 5 34 800]
 > (.tm #)
 [(tm) 3349/10]
 > (current-tm-delay old-current-tm-delay))


(TEST ;; subtitles-time
 > (show (string/location.tim "Hi"))
 (Error (make-source-error "Hi" "need exactly one comma" (list)))
 > (show (string/location.tim "Hi,23"))
 (Error (make-source-error "Hi" "need two ':' characters in hh:mm:ss" (list)))
 > (show (string/location.tim (source "Hi,23" (location '(f) (position 1 1)))))
 (Error
  (make-source-error
   (source* "Hi" (list 'f) 1 1)
   "need two ':' characters in hh:mm:ss" (list)))
 > (show (string/location.tim
           (source "10:bb:c,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "bb" (list 'f) 1 4)
                           "not a number" (list)))
 > (show (string/location.tim
           (source "10:13:22,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "f23" (list 'f) 1 10)
                           "not a number" (list)))
 > (show (string/location.tim "01:52:47,670"))
 (Ok (tim 1 52 47 670))

 ;; faulty formatting detection:
 > (show (string/location.tim "01:30:44,200"))
 (Ok (tim 1 30 44 200))
 > (show (string/location.tim "01:30:44,2"))
 (Error (make-source-error "2" "expecting string of length" (list 3)))
 > (show (string/location.tim "01:3:44,200"))
 (Error (make-source-error "3" "expecting string of length" (list 2))))

