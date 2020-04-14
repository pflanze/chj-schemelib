;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export latin1-char?
        latin1-string?)

(include "cj-standarddeclares.scm")


;; (def (latin1-init!)
;;      ;; create a u8vector from 0 .. 255
;;      (def latin1:set (list->u8vector (iota 256)))

;;      (call-with-output-file "latin1.txt"
;;        (C write-u8vector latin1:set _))

;;      (call-with-input-file (list path: "latin1.txt"
;;                                  char-encoding: 'ISO-8859-11)
;;        (lambda (p)
;;          (let lp ((res '()))
;;            (let (c (read-char p))
;;              (if (eof-object? c)
;;                  (=> (reverse res)
;;                      (.map .integer))
;;                  (lp (cons c res))))))))

;; (TEST
;;  > (equal? (latin1-init!) (iota 256))
;;  #t)

;; That was a joke. ?

(def (latin1-char? v)
     (and (char? v)
          (< (char->integer v) 256)))

(def (latin1-string? v)
     (and (string? v)
          (string-every latin1-char? v)))

(TEST
 > (latin1-string? "")
 #t
 > (latin1-string? "Hi")
 #t
 > (latin1-string? "Hi Wöé")
 #t
 > (latin1-string? (string (.char 255)))
 #t
 > (latin1-string? (string (.char 256)))
 #f
 > (string (.char 257))
 "ā"
 > (latin1-string? #)
 #f)


