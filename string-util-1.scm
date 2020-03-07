;;; Copyright 2010, 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-source source position position-line position-column-add)
         test
	 ;;(list-util-1 list-split)
	 )

(export string-contains-char?
	string-split
        string-split/location)

(include "cj-standarddeclares.scm")


(define (char-or-pred.pred char-or-pred)
  (cond ((char? char-or-pred)
         (lambda (c)
           (eq? c char-or-pred)))
        ((procedure? char-or-pred)
         char-or-pred)
        (else
         (error "expecting char or pred:" char-or-pred))))


(define (string-contains-char? str char-or-pred)
  (let ((len (string-length str))
        (pred (char-or-pred.pred char-or-pred)))
    (let lp ((i 0))
      (and (< i len)
	   (or (pred (string-ref str i))
	       (lp (inc i)))))))

(TEST
 > (string-contains-char? "Hello" char-newline?)
 #f
 > (string-contains-char? "Hello\n" char-newline?)
 #t
 )


;; (define (string-split str char-or-pred)
;;   (map list->string (list-split (string->list str) char-or-pred)))

(define (string-split str char-or-pred #!optional retain-matches?)
  (let ((len (string-length str))
	(pred (char-or-pred.pred char-or-pred)))
    (let lp ((i (dec len))
	     (prev-position len)
	     (strs '()))
      (if (>= i 0)
	  (if (pred (string-ref str i))
	      (lp (dec i)
		  i
		  (cons (substring str
				   (if retain-matches?
				       i
				       (inc i))
				   prev-position)
			strs))
	      (lp (dec i)
		  prev-position
		  strs))
	  (cons (substring str 0 prev-position) strs)))))

(TEST
 > (string-split "Foo|bar|baz|" #\x)
 ("Foo|bar|baz|")
 > (string-split "Foo|bar|baz|" #\|)
 ("Foo" "bar" "baz" "")
 > (string-split "|bar|baz|" #\|)
 ("" "bar" "baz" "")
 > (string-split "||baz|" #\|)
 ("" "" "baz" "")
 > (string-split "||baz|" (lambda (c) (case c ((#\|) #t) (else #f))) #f)
 ("" "" "baz" "")
 > (string-split "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))))
 ("" "" "b" "z" "")
 > (string-split "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))) #t)
 ("" "|" "|b" "az" "|")
 > (string-split "Hello\nWorld" #\newline #t)
 ;; Is this really what it should be? XX surely not?
 ("Hello" "\nWorld"))


(define (string-split/location str/location
                               char-or-pred
                               #!optional retain-matches?)
  "If str/location is source (with location), then return a list of
source, too, with each element's location updated to reflect the
position of the start of that element."
  (if (source? str/location)
      ;; adapted copy-paste; changed to process from the left.
      (let* ((str (source-code str/location))
             (loc (source-location str/location))
             (pos (location-position loc))
             (cnt (location-container loc))
             (len (string-length str))
             (pred (char-or-pred.pred char-or-pred)))
        (let lp ((pos pos)
                 (startpos pos)
                 (i 0)
                 (prev-i 0)
                 (rstrs '()))
          (if (< i len)
              (let* ((c (string-ref str i))
                     (pos* (case c
                             ((#\newline)
                              ;; Assume Unix behaviour.
                              (position (inc (position-line pos))
                                        1))
                             ((#\return)
                              ;; XX issue with MacOS (Classic only?)?
                              (position (position-line pos)
                                        1))
                             (else
                              ;; same for #\tab as well (since pos is
                              ;; in characters, not the visual
                              ;; column!)
                              (position-column-add pos 1)))))
                (if (pred c)
                    (lp pos*
                        pos*
                        (inc i)
                        (if retain-matches?
                            i
                            (inc i))
                        (cons (source (substring str
                                                 prev-i
                                                 i)
                                      (location cnt startpos))
                              rstrs))
                    (lp pos*
                        startpos
                        (inc i)
                        prev-i
                        rstrs)))
              (reverse
               (cons (source (substring str prev-i len)
                             ;; XX use pos here correct???
                             (location cnt startpos))
                     rstrs)))))
      (string-split str/location char-or-pred retain-matches?)))

(TEST
 > (define (t . args)
     (let ((ss (apply string-split/location args)))
       (list (map source-code ss)
             (map (lambda (v) (location-string (source-location v))) ss))))
 > (define s (source "Hello\nworld" (location '(foo) (position 10 13))))
 > (t s #\newline)
 (("Hello" "world") ("(foo)@10.13" "(foo)@11.1"))
 > (t s #\o)
 (("Hell" "\nw" "rld") ("(foo)@10.13" "(foo)@10.18" "(foo)@11.3"))
 > (t s #\newline #t)
 (("Hello" "\nworld") ("(foo)@10.13" "(foo)@11.1"))
 > (string-split/location "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))) #t)
 ("" "|" "|b" "az" "|")
 > (define s2 (source "||baz|" (location '(f) (position 10 13))))
 > (t s2 (lambda (c) (case c ((#\|) #t) (else #f))) #f)
 (("" "" "baz" "")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.19"))
 > (t s2 (lambda (c) (case c ((#\|) #t) (else #f))) #t)
 (("" "|" "|baz" "|")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.19"))
 ;; > (t s2 (lambda (c) (case c ((#\| #\a) #t) (else #f))) #t)
 ;; (("" "|" "|b" "az" "|")
 ;;  ("(f)@10.13" "(f)@10.13" "(f)@10.15" "(f)@10.16" "(f)@10.18"))
 )

