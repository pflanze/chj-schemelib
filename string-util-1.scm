;;; Copyright 2010, 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-source source position position-line position-column-add)
         (srfi-13-kmp string-contains)
         test)

(export string-contains-char?
	string-split
        string-split/location
        string-rmatches
        string-matches
        #!optional
        position-update-for-char
        position-update-in-string
        string-split-on-string
        string-split/left+right
        string-split/location/left+right)

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


(define (position-update-for-char pos c)
  (case c
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
     (position-column-add pos 1))))

(define (position-update-in-string pos str i0 i1)
  (let lp ((pos pos)
           (i i0))
    (if (< i i1)
        (lp (position-update-for-char pos (string-ref str i))
            (inc i))
        pos)))

(TEST
 > (define (t from to)
     (position-string
      (position-update-in-string (position 10 7) "Hello\nWorld.\n" from to)))
 > (t 0 1)
 "10.8"
 > (t 1 1)
 "10.7"
 > (t 1 2)
 "10.8"
 > (t 4 5)
 "10.8"
 > (t 4 6)
 "11.1")


;; Should have taken this as basis for string-split* in all cases,
;; then duplication could be saved / much simpler code (yes, would
;; allocate more, though).

(define (string-rmatches str pat #!optional ci? maybe-pos)
  "Returns a reversed list of non-overlapping matches of `pat` in
`str`, `(list str-i)`, where `str-i` determines the location of the
start of the pattern. If `maybe-pos` is given, `(list (cons str-i
pos))` is returned instead."
  (let* ((len (string-length str))
         (patlen (string-length pat))
         (contains (if ci? string-contains-ci string-contains)))
    (let lp ((maybe-pos maybe-pos)
             (i 0)
             (rmatches '()))
      (cond ((contains str pat i)
             => (lambda (i*)
                  (if maybe-pos
                      (let ((maybe-pos* (position-update-in-string
                                         maybe-pos str i i*)))
                        (lp (position-update-in-string
                             maybe-pos* pat 0 patlen)
                            (+ i* patlen)
                            (cons (cons i* maybe-pos*) rmatches)))
                      (lp maybe-pos ;; #f
                          (+ i* patlen)
                          (cons i* rmatches)))))
            (else
             rmatches)))))

(define (string-matches str pat #!optional ci? maybe-pos)
  (reverse (string-rmatches str pat ci? maybe-pos)))

(TEST
 > (string-rmatches "Hello World" "ll" #f)
 (2)
 > (string-rmatches "Hello World" "o" #f)
 (7 4)
 > (string-rmatches "Hello World" "O")
 ()
 > (string-matches "Hello World" "O" #t)
 (4 7)
 > (string-matches "abcbcb ebcb." "bcb")
 (1 8)
 > (define (t1 . args)
     (map (lambda (i+pos) (list (car i+pos)
                           (position-string (cdr i+pos))))
          (apply string-matches args)))
 > (t1 "abcbcb\n ebcb." "bcb" #f (position 1 1))
 ((1 "1.2") (9 "2.3"))
 > (t1 "Hello\nWorld" "l" #f (position 1 1))
 ((2 "1.3") (3 "1.4") (9 "2.4")))


(define (string-split-on-string str/location pat left? right? ci?)
  (let* ((str (source-code str/location))
         (maybe-loc (maybe-source-location str/location))
         (maybe-pos (and maybe-loc
                         (location-position maybe-loc)))
         (patlen (string-length pat)))
    (let lp ((rmatches (string-rmatches str pat ci? maybe-pos))
             (prev-i (string-length str))
             (parts '()))
      (let ((sub
             (lambda (i maybe-pos)
               (let ((s (substring str
                                   i
                                   prev-i)))
                 (if maybe-pos
                     (source s
                             (location (location-container maybe-loc)
                                       maybe-pos))
                     s)))))
        (if (null? rmatches)
            (cons (sub 0 maybe-pos) parts)
            (let ((m (car rmatches)) (rmatches* (cdr rmatches)))
              (let ((i (if (pair? m) (car m) m))
                    (maybe-pos (and (pair? m) (cdr m))))
                (lp rmatches*
                    (if left? (+ i patlen) i)
                    (cons (sub (if right? i (+ i patlen))
                               (and maybe-pos
                                    ;; could optimize into a
                                    ;; previously extracted position
                                    ;; operation
                                    (position-update-in-string
                                     maybe-pos pat 0 patlen)))
                          parts)))))))))

(TEST
 > (string-split-on-string "Hello Worlllds" "ll" #f #f #f)
 ("He" "o Wor" "lds")
 > (string-split-on-string "Hello Worlllds" "Ll" #f #f #f)
 ("Hello Worlllds")
 > (string-split-on-string "Hello Worlllds" "Ll" #f #f #t)
 ("He" "o Wor" "lds")
 > (string-split-on-string "Hello Worlllds" "ll" #f #t #f)
 ("He" "llo Wor" "lllds")
 > (string-split-on-string "Hello Worlllds" "ll" #t #f #f)
 ("Hell" "o Worll" "lds")
 > (string-split-on-string "Hello Worlllds" "ll" #t #t #f)
 ("Hell" "llo Worll" "lllds"))



;; (define (string-split str char-or-pred)
;;   (map list->string (list-split (string->list str) char-or-pred)))

(define (string-split/left+right str char-or-str-or-pred left? right?
                                 #!optional ci?)
  (if (string? char-or-str-or-pred)
      (string-split-on-string str char-or-str-or-pred left? right? ci?)
      (let ((len (string-length str))
            (pred (char-or-pred.pred char-or-str-or-pred)))
        (let lp ((i (dec len))
                 (prev-position len)
                 (strs '()))
          (if (>= i 0)
              (if (pred (string-ref str i))
                  (lp (dec i)
                      (if left?
                          (inc i)
                          i)
                      (cons (substring str
                                       (if right?
                                           i
                                           (inc i))
                                       prev-position)
                            strs))
                  (lp (dec i)
                      prev-position
                      strs))
              (cons (substring str 0 prev-position) strs))))))

(define (string-util-1#retain-matches-to _/left+right
                                         str char-or-pred retain-matches
                                         ci?)
  (let ((cont
         (lambda (left? right?)
           (_/left+right str char-or-pred left? right? ci?))))
    (case retain-matches
      ((right) (cont #f #t))
      ((left) (cont #t #f))
      ((#f) (cont #f #f))
      (else
       (error "retain-matches must be #f, 'right or 'left:"
              retain-matches)))))

(define (string-split str char-or-pred #!optional retain-matches ci?)
  "If char-or-pred is a function, it is expected to take a char and
return a boolean. If retain-matches is #f, the matching character is
dropped, if retain-matches is 'right, the matching character is added
to the element to its right, for 'left to the element to its left."
  (string-util-1#retain-matches-to string-split/left+right
                                   str char-or-pred retain-matches
                                   ci?))

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
 > (string-split "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))) 'right)
 ("" "|" "|b" "az" "|")
 > (string-split "Hello\nWorld" #\newline 'right)
 ("Hello" "\nWorld")
 > (string-split "Hello\nWorld" #\newline 'left)
 ("Hello\n" "World")
 > (string-split "Hello\nWorld\n" #\newline 'left)
 ;; Ok?
 ("Hello\n" "World\n" "")

 ;; String matching
 > (string-split "Hello\nWorld" "\n" 'left)
 ("Hello\n" "World")
 > (string-split "Hello\nWorld" "\n")
 ("Hello" "World")
 > (string-split "Hello\nWorld" "\nW")
 ("Hello" "orld")
 > (string-split "Hello\nWorld" "\nw")
 ("Hello\nWorld")
 > (string-split "Hello\nWorld" "\nw" #f #t)
 ("Hello" "orld"))


(define (string-split/location/left+right str/location
                                          char-or-str-or-pred
                                          left? right?
                                          #!optional ci?)
  (if (source? str/location)
      (if (string? char-or-str-or-pred)
          (string-split-on-string str/location
                                  char-or-str-or-pred
                                  left? right? ci?)

          ;; adapted copy-paste; changed to process from the left.
          (let* ((str (source-code str/location))
                 (loc (source-location str/location))
                 (pos (location-position loc))
                 (cnt (location-container loc))
                 (len (string-length str))
                 (pred (char-or-pred.pred char-or-str-or-pred)))
            (let lp ((pos pos)
                     (startpos pos)
                     (i 0)
                     (prev-i 0)
                     (rstrs '()))
              (if (< i len)
                  (let* ((c (string-ref str i))
                         (pos* (position-update-for-char pos c)))
                    (if (pred c)
                        (lp pos*
                            pos*
                            (inc i)
                            (if right?
                                i
                                (inc i))
                            (cons (source (substring str
                                                     prev-i
                                                     (if left?
                                                         (inc i)
                                                         i))
                                          (location cnt startpos))
                                  rstrs))
                        (lp pos*
                            startpos
                            (inc i)
                            prev-i
                            rstrs)))
                  (reverse
                   (cons (source (substring str prev-i len)
                                 (location cnt startpos))
                         rstrs))))))
      (string-split/left+right str/location
                               char-or-str-or-pred
                               left? right? ci?)))

(define (string-split/location str/location
                               char-or-str-or-pred
                               #!optional retain-matches ci?)
  "Same as string-split, but if str/location is source (with
location), then return a list of source, too, with each element's
location updated to reflect the position of the start of that
element."
  (string-util-1#retain-matches-to string-split/location/left+right
                                   str/location
                                   char-or-str-or-pred
                                   retain-matches
                                   ci?))

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
 > (t s #\newline 'right)
 (("Hello" "\nworld") ("(foo)@10.13" "(foo)@11.1"))
 > (t s #\newline 'left)
 (("Hello\n" "world") ("(foo)@10.13" "(foo)@11.1"))

 ;; string matching:
 > (t s "o")
 (("Hell" "\nw" "rld")
  ("(foo)@10.13" "(foo)@10.18" "(foo)@11.3"))
 > (t s "\nW")
 (("Hello\nworld") ("(foo)@10.13"))
 > (t s "\nW" #f #t)
 (("Hello" "orld") ("(foo)@10.13" "(foo)@11.2")) 
 > (t s "\nW" 'left #t)
 (("Hello\nw" "orld") ("(foo)@10.13" "(foo)@11.2"))
 > (t s "\nW" 'right #t)
 ;; same as with char based splitting, the found location refers to
 ;; the field, does not include the separator
 (("Hello" "\nworld") ("(foo)@10.13" "(foo)@11.2"))
 > (t s "\n")
 (("Hello" "world") ("(foo)@10.13" "(foo)@11.1"))
 > (t s "l")
 (("He" "" "o\nwor" "d")
  ("(foo)@10.13" "(foo)@10.16" "(foo)@10.17" "(foo)@11.5"))

 > (string-split/location "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f)))
                          'right)
 ("" "|" "|b" "az" "|")

 > (define s2 (source "||baz|" (location '(f) (position 10 13))))
 ;; "||baz|"
 ;;  3456789
 > (t s2 (lambda (c) (case c ((#\|) #t) (else #f))) #f)
 (("" "" "baz" "")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.19"))
 > (t s2 (lambda (c) (case c ((#\|) #t) (else #f))) 'left)
 (("|" "|" "baz|" "")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.19"))
 > (t s2 (lambda (c) (case c ((#\|) #t) (else #f))) 'right)
 (("" "|" "|baz" "|")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.19"))

 > (t s2 (lambda (c) (case c ((#\| #\a) #t) (else #f))) 'left)
 (("|" "|" "ba" "z|" "")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.17" "(f)@10.19"))
 > (t s2 (lambda (c) (case c ((#\| #\a) #t) (else #f))) 'right)
 (("" "|" "|b" "az" "|")
  ("(f)@10.13" "(f)@10.14" "(f)@10.15" "(f)@10.17" "(f)@10.19")))

