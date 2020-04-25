;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export (methods chars.remove-html-markup
                 string.remove-html-markup)
        #!optional
        chars?)

"Remove html markup. Simpler than an HTML parser: doesn't figure out
the correct tag nesting, and doesn't even check for proper syntax
within tags."

(include "cj-standarddeclares.scm")



;; unlike char-list? which is list-of, OK? XX move and make consistent
(def chars? (ilist-of char?))

(def. (chars.remove-html-markup cs)
  (let outer ((cs cs))
    (if-let-pair ((c cs*) cs)
                 (case c
                   ((#\<)
                    (let lp ((cs cs*))
                      (if-let-pair ((c cs*) cs)
                                   (case c
                                     ((#\>)
                                      (outer cs*))
                                     ((#\<)
                                      (error "'<' after '<'"))
                                     (else
                                      (lp cs*)))
                                   (error "missing '>' after '<'"))))
                   (else
                    (cons c (outer cs*))))
                 '())))

(def. string.remove-html-markup
  (=>* string.list
       chars.remove-html-markup
       char-list.string))

(TEST
 > (.remove-html-markup "")
 ""
 > (.remove-html-markup "123  f")
 "123  f"
 > (.remove-html-markup "123  <f>")
 "123  "
 > (.remove-html-markup "123  <f>5</f>")
 "123  5"
 > (.remove-html-markup "123  f>5</f>")
 "123  f>5" ;; ok?
 > (%try (.remove-html-markup "123  <f>5</"))
 (exception text: "missing '>' after '<'\n")
 > (%try (.remove-html-markup "123  <f>5</<"))
 (exception text: "'<' after '<'\n"))

