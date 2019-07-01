;;; Copyright 2007 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
	 (srfi-1 fold fold-right)
	 (cj-string string-copy! @string-copy!_end)
	 test
	 test-lib-1)

(export flat-append-strings
	#!optional
	flat-string-length)


; (define (flat-append-strings . strings-or-lists-of-strings)
;   (apply string-append (flatten strings-or-lists-of-strings)))

; or, much lengthier but w/o intermediate list building:

(define (flat-string-length lists-of-strings
			    #!optional
			    (tot 0)
			    do! ;; (& #!void)
			    )
  (let flat-string-length ((lists-of-strings lists-of-strings)
			   (tot tot))
    (if (null? lists-of-strings)
	tot
	(fold (lambda (v tot)
		(cond ((pair? v)
		       (flat-string-length v tot))
		      ((null? v)
		       tot)
		      ((string? v)
		       (when do! (do! v tot))
		       (+ tot (string-length v)))
		      (else
		       (error "flat-string-length: not a string or list:" v))))
	      tot
	      lists-of-strings))))

(TEST
 > (flat-string-length '() 1)
 1
 > (flat-string-length '((())) 1)
 1
 > (flat-string-length '("he" "l" () "lo" ()) -1)
 4
 > (flat-string-length '(() ((("hel")) "l") "o") 0)
 5)


(define (flat-append-strings . strings-or-lists-of-strings)
  (let* ((len (flat-string-length strings-or-lists-of-strings 0))
	 (str (##make-string len)))
    (flat-string-length
     strings-or-lists-of-strings 0
     (lambda (v pos)
       ;; (string-copy! str pos v 0 (string-length v))
       (@string-copy!_end str pos v 0 (##string-length v))
       ))
    str))

(TEST
 > (flat-append-strings '("Hallo"))
 "Hallo"
 > (flat-append-strings '("Hallo"(() " W"("e")"lt")))
 "Hallo Welt"
 > (flat-append-strings '(()))
 ""
 > (%try (flat-append-strings '(() #f)))
 (exception text: "flat-string-length: not a string or list: #f\n"))

