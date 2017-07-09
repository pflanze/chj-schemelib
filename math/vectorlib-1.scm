;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cc CC)
	 math/vectorlib)

(include "../cj-standarddeclares.scm")


;; === write PNM/PGM

(define (Mr.print-pgm m #!optional width height invert?)
  (define port (current-output-port))
  (println "P5")
  (letv
   ((s0 s1) (Mr.sizes m))
   (let ((stretchheight (if height (max (round (/ height s0)) 1) 1))
	 (stretchwidth (if width (max (round (/ width s1)) 1) 1)))
     (let ((h (* s0 stretchheight))
	   (w (* s1 stretchwidth)))
       (println w " " h)
       (println "255")
       (letv ((lo hi) (Mr.min+max m))
	     (let* ((range (- hi lo))
		    (rangechange (/ 255. range)))
	       (declare (fixnum) (not safe))
	       ;; assumption again on format
	       (for..<
		(i0 0 s0)
		(repeat
		 stretchheight
		 (for..<
		  (i1 0 s1)
		  (let* ((val (CC (integer
				   (fl* (fl- (Mr.ref@ m i0 i1) lo) rangechange))))
			 (val (if invert? (- 255 val) val)))
		    ;;(assert (<= 0 val 255))
		    (repeat
		     stretchwidth
		     (##write-u8 val port))))))))))))

(define. (Mr.write-pgm-file m path #!optional width height invert?)
  (with-output-to-file path (thunk (Mr.print-pgm m width height invert?))))

(define tmpcount 0)

(define. (Mr.imagesc m #!optional invert?)
  (let ((c tmpcount))
    (set! tmpcount (inc tmpcount))
    (let ((path (string-append "~/tmp/imagesc-"
			       (number->string c)
			       ".pgm")))
      (.write-pgm-file m path 400 400 invert?)
      ;;XXX param?
      (future (shell-command (string-append "display " path))))))

(define (view m #!optional invert?)
  (fut (.imagesc m invert?)))


