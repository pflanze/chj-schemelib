;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles
         html-remove
         )

(export (methods strings.save-to!)
        T-no-add
        i
        (methods chars.subtitle-strip-newlines
                 string.subtitle-strip-newlines)
        #!optional
        strings?)

(include "cj-standarddeclares.scm")



(def strings? (ilist-of string?))
(def. (strings.save-to! vs [path-or-port-settings? pops] #!optional encoding)
  ;; ignore encoding, OK? Always want it in default (UTF-8) for this.
  (=> vs
      (.map (=>* string.remove-html-markup
                 (string-split char-whitespace?)
                 (.filter (complement string-empty?))))
      flatten1
      ((lambda (ss)
         (call-with-output-file pops
           (lambda (p)
             (.for-each ss (C displayln _ p))))))))


(def (T-no-add n)
     (lambda (v)
       (if (subtitles-item? v)
           (=> v
               (.no-update (C + _ n)))
           v)))

(def subtitles-utils#i
     (=>* (string.split #\newline)
          (.map (lambda (line)
                  (if (string.null? (trim-both line))
                      line
                      ($ "<i>${line}</i>"))))
          (strings-join "\n")))

(TEST
 > (##namespace ("subtitles-utils#" i))
 > (i "fun")
 "<i>fun</i>"
 > (i "fun \n")
 "<i>fun </i>\n"
 > (i "fun \nThere")
 "<i>fun </i>\n<i>There</i>")


(def. (chars.subtitle-strip-newlines l [boolean? nbsp-after-minus?])
  (reverse
   (let lp ((l l)
            (prev-space? #f)
            (r '()))
     (def (process- l r)
          "process part after #\\- character"
          (let (r* (cons #\- r))
            (if nbsp-after-minus?
                (let lp- ((l l)
                          (prev-space? prev-space?)
                          (r r*))
                  (if-let-pair
                   ((a l*) l)
                   (if (eq? a #\space)
                       (lp- l* #t (cons nbsp r))
                       (lp l #f r))
                   r))
                (lp l prev-space? r*))))
     (if-let-pair
      ((a l*) l)
      (cond ((eq? a #\-)
             (process- l* r))
            ((eq? a #\newline)
             (if-let-pair
              ((b l**) l*)
              (if (eq? b #\-)
                  ;; leave newline in and process #\-
                  (process- l** (cons a r))
                  ;; replace newline with a space, unless previous
                  ;; character was a space already
                  (lp l*
                      ;; consider newline a space regardless of
                      ;; whether it was replaced by a space? XX make a
                      ;; whitespace flag, fully?
                      #t
                      (if prev-space? r (cons #\space r))))
              (cons a r)))
            (else
             (lp l* (eq? a #\space) (cons a r))))
      r))))

(def. (string.subtitle-strip-newlines str [boolean? nbsp-after-minus?])
     "Strip unnecessary newlines from a subtitle segment string; if
the second argument is true, will replace spaces between a `-` and the
next non-space character with non-breaking spaces."
     ;; good that I don't keep them split up?
     (=> str
         .list
         (.subtitle-strip-newlines nbsp-after-minus?)
         char-list.string))
;; BTW:
;;  .list -> .char-list or .chars ?
;;  char-list.string -> chars.string ?


(TEST
 > (def t (C .subtitle-strip-newlines _ #t))
 > (t "a b")
 "a b"
 > (t "a\nb")
 "a b"
 > (t "a \nb")
 "a b"
 > (t "a \n b")
 "a  b"
 > (t "\na\n")
 ;; should it remove the \n to the left?
 " a\n"
 > (t "\n\na\n\n")
 " a \n"
 > (t "\nder uns  sagen\nkonnte, \ndass die Früchte verdorben waren.\n")
 " der uns  sagen konnte, dass die Früchte verdorben waren.\n"
 > (t "Kriege ich den Schlüssel \nvon\nIhnen, Sir?\n- Ja,\nich werde ihn übergeben. -  Gut so!")
 "Kriege ich den Schlüssel von Ihnen, Sir?\n- Ja, ich werde ihn übergeben. -  Gut so!")

