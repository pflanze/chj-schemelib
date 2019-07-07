;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 utf8
	 u8vector0
	 (string-util-1 string-split)
	 test
	 )

;; http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt


(def (xu8 str)
     (apply u8vector (fold-right
		      (lambda (s t)
			(let ((len (string-length s)))
			  (xcase len
				 ((0) t)
				 ((2) (cons (string->number s 16) t))
				 ((4) (cons* (string->number (substring s 0 2) 16)
					     (string->number (substring s 2 4) 16)
					     t)))))
		      '()
		      (string-split str #\space))))

(def (utf8-test [ucs4-codepoint? codepoint] vec)
     ;; (assert (equal? (string (integer->char codepoint))
     ;; 		     (u8vector.utf8-parse u8vec)))
     ;; OK that fails with an exception from Gambit, (integer->char
     ;; 2097152), out of range. Thus:
     (let ((ps (u8vector.utf8-codepoints vec)))
       ;; ^ .utf8-codepoints doesn't work as u8vector0? matches the
       ;;   2.1.1 case
       (if (one-item? ps)
	   (if (= (car ps) codepoint)
	       vec
	       (begin
		 ;; XX wanted to use WARN, what's the state with that?
		 (warn "test failure, non-matching code point:" codepoint vec ps)
		 vec))
	   (begin
	     ;; XX wanted to use WARN, what's the state with that?
	     (warn "test failure, expecting 1 code point:" codepoint vec ps)
	     vec))))


(def 2.1.1 (utf8-test #x00000000 (xu8 "00")))
(def 2.1.2 (utf8-test #x00000080 (xu8 "c2 80")))
(def 2.1.3 (utf8-test #x00000800 (xu8 "e0 a0 80")))
(def 2.1.4 (utf8-test #x00010000 (xu8 "f0 90 80 80")))
(def 2.1.5 (utf8-test #x00200000 (xu8 "f8 88 80 80 80")))
(def 2.1.6 (utf8-test #x04000000 (xu8 "fc 84 80 80 80 80")))
(def 2.2.1 (utf8-test #x0000007F (xu8 "7f")))
(def 2.2.2 (utf8-test #x000007FF (xu8 "df bf")))
(def 2.2.3 (utf8-test #x0000FFFF (xu8 "ef bf bf")))
(def 2.2.4 (utf8-test #x001FFFFF (xu8 "f7 bf bf bf")))
(def 2.2.5 (utf8-test #x03FFFFFF (xu8 "fb bf bf bf bf")))
(def 2.2.6 (utf8-test #x7FFFFFFF (xu8 "fd bf bf bf bf bf")))
(def 2.3.1 (utf8-test #x0000D7FF (xu8 "ed 9f bf")))
(def 2.3.2 (utf8-test #x0000E000 (xu8 "ee 80 80")))
(def 2.3.3 (utf8-test #x0000FFFD (xu8 "ef bf bd")))
(def 2.3.4 (utf8-test #x0010FFFF (xu8 "f4 8f bf bf")))
(def 2.3.5 (utf8-test #x00110000 (xu8 "f4 90 80 80")))
(def 3.1.1 (xu8 "80"))
(def 3.1.2 (xu8 "bf"))
(def 3.1.3 (xu8 "80 bf"))
(def 3.1.4 (xu8 "80 bf 80"))
(def 3.1.5 (xu8 "80 bf 80 bf"))
(def 3.1.6 (xu8 "80 bf 80 bf 80"))
(def 3.1.7 (xu8 "80 bf 80 bf 80 bf"))
(def 3.1.8 (xu8 "80 bf 80 bf 80 bf 80"))

(def 3.1.1-8 (list 3.1.1 3.1.2 3.1.3 3.1.4 3.1.5 3.1.6 3.1.7 3.1.8))

(def 3.1.9 (list (xu8 "8081 8283 8485 8687 8889 8a8b 8c8d 8e8f ")
		 (xu8 "9091 9293 9495 9697 9899 9a9b 9c9d 9e9f")
		 (xu8 "a0a1 a2a3 a4a5 a6a7 a8a9 aaab acad aeaf")
		 (xu8 "b0b1 b2b3 b4b5 b6b7 b8b9 babb bcbd bebf")))
(def 3.2.1 (list (xu8 "c020 c120 c220 c320 c420 c520 c620 c720 c820 c920 ca20 cb20 cc20 cd20 ce20 cf20")
		 (xu8 "d020 d120 d220 d320 d420 d520 d620 d720 d820 d920 da20 db20 dc20 dd20 de20 df20")))

(TEST
 > (def t (lambda (v) (%try (u8vector.utf8-codepoints v))))
 > (map t 3.1.9)
 ;; XX should it really throw exceptions? Return a Result instead?
 ((exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n"))
 ;; XX also, offer an option to let it re-synchronize!
 > (map t 3.1.1-8)
 ((exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")
  (exception text: "utf-8 decoding error\n")))


;; XX continue once functions to re-synchronize-and-continue are
;; available...:

;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
;; (def  (xu8 ""))
