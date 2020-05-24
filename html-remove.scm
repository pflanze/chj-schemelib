;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (list-util-3 charlist-ci-starts-with?)
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

(def _charlist-ci-starts-with? (list-starts-with?/equal? char-ci=?))
;; and give it a nicer API:
(def (charlist-ci-starts-with? lis sublis) -> (maybe ilist?)
     "Returns the remainder after sublis if lis starts with sublis."
     (letv ((? rest) (_charlist-ci-starts-with? lis sublis))
           (and ? rest)))

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
                   ((#\&)
                    (cond ((charlist-ci-starts-with? cs* '(#\l #\t #\;))
                           => (lambda (rest) (cons #\< (outer rest))))
                          ((charlist-ci-starts-with? cs* '(#\g #\t #\;))
                           => (lambda (rest) (cons #\> (outer rest))))
                          ((charlist-ci-starts-with? cs* '(#\a #\m #\p #\;))
                           => (lambda (rest) (cons #\& (outer rest))))
                          (else
                           (cons c (outer cs*)))))
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
 (exception text: "'<' after '<'\n")
 > (.remove-html-markup "123 &lt;&LT;&amp;&amp;&amp <f &lt;> &GT;&gt; ")
 "123 <<&&&amp  >> ")


