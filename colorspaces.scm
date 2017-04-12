;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; (Perhaps should read
;; http://polymathprogrammer.com/2008/08/04/basic-colour-theory-for-programmers/
;; / https://news.ycombinator.com/item?id=8574032)

(require easy
	 test
	 math-approximate
	 (rgb-util rgb:0..1?))


;; http://en.wikipedia.org/wiki/Rec._709

;; ITU-R Recommendation BT.709, more commonly known by the
;; abbreviations Rec. 709 or BT.709, standardizes the format of
;; high-definition television, having 16:9 (widescreen) aspect
;; ratio. The first edition of the standard was approved in 1990.


(def (rec709:lum.transfer #(rgb:0..1? l)) -> rgb:0..1?
     (if (< l 0.018)
	 (* 4.500 l)
	 (- (* 1.099 (expt l 0.45)) 0.099)))

;; odd really exactly the wrong way round? more of the digital part in
;; l is devoted to the higher, relatively closer brightnesses?
;; luminosities. ?


(def rec709:transfer.lum
     ;; 1000000. or 256.? 
     (inverse rec709:lum.transfer -0.0001 1.0001))

(TEST
 > (inexact.round-at (rec709:transfer.lum 0) 7)
 0.
 > (inexact.round-at (rec709:transfer.lum 0.1) 6)
 .022428
 > (inexact.round-at (rec709:transfer.lum 0.5) 6)
 .259589
 > (inexact.round-at (rec709:transfer.lum 1) 6)
 1.

 > (inexact.round-at (rec709:lum.transfer 0.) 7)
 0.
 > (inexact.round-at (rec709:lum.transfer .02242793595790863) 6)
 0.1
 > (inexact.round-at (rec709:lum.transfer .9999999837875365) 6)
 1.
 )


;; http://en.wikipedia.org/wiki/SRGB

;; sRGB is a standard RGB color space created cooperatively by HP and
;; Microsoft in 1996 for use on monitors, printers and the Internet.

;; sRGB uses the ITU-R BT.709 primaries, the same as are used in
;; studio monitors and HDTV,[1] and a transfer function (gamma curve)
;; typical of CRTs. This specification allowed sRGB to be directly
;; displayed on typical CRT monitors of the time, a factor which
;; greatly aided its acceptance.

;; Unlike most other RGB color spaces, the sRGB gamma cannot be
;; expressed as a single numerical value. The overall gamma is
;; approximately 2.2, consisting of a linear (gamma 1.0) section near
;; black, and a non-linear section elsewhere involving a 2.4 exponent
;; and a gamma (slope of log output versus log input) changing from
;; 1.0 through about 2.3.

(def srgb:a 0.055)

;; |linear| is proportional to amount of light energy, right?
(def (srgb:lum.transfer #(rgb:0..1? linear)) -> rgb:0..1?
     (if (< linear 0)
	 0
	 (if (<= linear 0.0031308)
	     (* 12.92 linear)
	     (- (* (+ 1 srgb:a) (expt linear (/ 2.4))) srgb:a))))

;; (plot (list srgb:lum.transfer rec709:lum.transfer (onthese-2 srgb:lum.transfer rec709:lum.transfer -)) 0 1)
;; srgb is more strongly curved than rec709

(def srgb:transfer.lum
     (inverse srgb:lum.transfer -0.0001 1.0001))

