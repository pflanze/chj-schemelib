;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         (simple-match-1 assert*)
         srfi-1
         char-util
         test)

(export (macro string-interpolate)
        (macro $$))

(include "cj-standarddeclares.scm")

(define (string-interpolate:variable-char? c)
  (or (char-alphanumeric? c)
      ;; do not allow '!', '.', ',', as those shouldn't be likely ('!'
      ;; and '.'  being used for procedure names hence unprintable)
      ;; and easily used as english punctuation. Similarly, '@',
      ;; ':'. '=' is used for "$a=$b" style stuff, and '/' for
      ;; "$a/$b"..
      ((char-one-of?/ "-+<>*#?") c)))

(define (string-interpolate:expand/ converter-expr-fn)
  (lambda (expr* tail)
    (let ((expr (source-code expr*)))
      (if (string? expr)
          (let* ((str expr)
                 (len (string-length str)))
            (define (err rest msg . args)
              (apply raise-source-error expr*
                     (string-append "(at char pos "
                                    (number->string (- len (length rest)))
                                    ") "
                                    msg)
                     args))
            (let lp ((cs (string->list str))
                     (rcs '())
                     (fragments '()))
              (let* ((fragments*
                      (lambda ()
                        (if (null? rcs)
                            fragments
                            (cons (list->string
                                   (reverse rcs))
                                  fragments))))
                     (lp-cs (lambda (cs var)
                              (lp cs
                                  '()
                                  (cons (converter-expr-fn
                                         (string->symbol
                                          (list->string var)))
                                        (fragments*))))))
                (if (null? cs)
                    (rappend (fragments*) tail)
                    (let-pair
                     ((c cs) cs)
                     (case c
                       ((#\$)
                        (if (null? cs)
                            (err cs "need variable name after $")
                            (let-pair
                             ((c cs*) cs)
                             (case c
                               ((#\{)
                                ;; even empty variable name is OK;
                                ;; even allow '{'. Handle escaping
                                ;; here, too.
                                (let getvar ((rvar '())
                                             (cs cs*))
                                  (if (null? cs)
                                      ;; say "non-escaped"?
                                      (err cs "missing '}' after '${'")
                                      (let-pair
                                       ((c cs) cs)
                                       (case c
                                         ((#\})
                                          (lp-cs cs (reverse rvar)))
                                         ((#\\)
                                          (if (null? cs)
                                              (err cs "need character after \\ within { }")
                                              (let-pair
                                               ((c cs) cs)
                                               (getvar (cons c rvar) cs))))
                                         (else
                                          (getvar (cons c rvar)
                                                  cs)))))))

                               ;; XX case $( )

                               (else
                                (let* ((var (take-while
                                             string-interpolate:variable-char? cs))
                                       (cs (drop cs (length var))))
                                  (if (null? var)
                                      (err cs "invalid variable name after $ -- use ${ } for names containing unusual characters")
                                      (lp-cs cs var))))))))
                       ((#\\)
                        (if (null? cs)
                            (err cs "need character after \\")
                            (let-pair
                             ((c cs) cs)
                             (lp cs (cons c rcs) fragments))))
                       (else
                        (lp cs (cons c rcs) fragments))))))))
          (cons (converter-expr-fn expr*) tail)))))

(define (string-interpolate-expand-with converter-fn-expr exprs)
  (let ((converter (if converter-fn-expr
                       (lambda (e)
                         `(,converter-fn-expr ,e))
                       identity)))
    `(string-append
      ,@(fold-right (string-interpolate:expand/ converter)
                    '()
                    exprs))))


(TEST
 > (define (t . vs)
     (string-interpolate-expand-with `.string vs))
 > (with-exception-catcher source-error-message (& (t "foo $ ")))
 "(at char pos 5) invalid variable name after $ -- use ${ } for names containing unusual characters"
 > (with-exception-catcher source-error-message (& (t "foo $")))
 "(at char pos 5) need variable name after $"
 > (with-exception-catcher source-error-message (& (t "foo $ {abc} d")))
 "(at char pos 5) invalid variable name after $ -- use ${ } for names containing unusual characters"
 > (with-exception-catcher source-error-message (& (t "foo ${abc d")))
 "(at char pos 11) missing '}' after '${'"
 
 > (t "foo $a")
 (string-append "foo " (.string a))
 > (t "foo $abc d")
 (string-append "foo " (.string abc) " d")
 > (t "foo ${abc} d")
 (string-append "foo " (.string abc) " d")
 > (t "foo ${abc }d")
 (string-append "foo " (.string |abc |) "d")
 > (t "foo ${abc{ }d")
 ;; XX worrysome, really allow?
 (string-append "foo " (.string |abc{ |) "d"))

(define-macro* (string-interpolate converter-fn . exprs)
  (string-interpolate-expand-with converter-fn exprs))


(define (string-interpolate:to-string v)
  (cond ((string? v) v)
        ((number? v) (number->string v))
        (else
         (error "string-interpolate:to-string: don't know how to handle:"
                v))))

(define-macro* ($$ . exprs)
  (string-interpolate-expand-with 'string-interpolate:to-string
                                  exprs))

(TEST
 > (define bar-world 11)
 > (with-exception-catcher type-exception?
                           (& (string-interpolate identity
                                                  "foo $bar-world, you")))
 #t
 > (string-interpolate number->string "foo $bar-world, you")
 "foo 11, you"
 > ($$ "foo $bar-world, you")
 "foo 11, you"
 > (define world "World")
 > (string-interpolate identity "Hello $world!")
 "Hello World!"
 > ($$ "Hello $world!")
 "Hello World!"
 > ($$ "Hello \\$world!")
 "Hello $world!"
 > ($$ "Hel\\lo\\\\$world \\$world!")
 "Hello\\World $world!"
 > (define |a}b| 10)
 > (define |a| "A")
 > ($$ "Hello ${a}b} ${a\\}b}!")
 "Hello Ab} 10!"
 > (with-exception-catcher source-error-message (& (eval '($$ "${foo\\"))))
 "(at char pos 6) need character after \\ within { }"
 > (with-exception-catcher source-error-message (& (eval '($$ "${foo\\f"))))
 "(at char pos 7) missing '}' after '${'"
 > (define wo "hm")
 > ($$ "Hello $wo\\rld!")
 ;; hm, OK to let this "slide", since, makes search/replace of
 ;; variable names more difficult if supported, and, using this trick
 ;; to cut off variable name might actually come in handy (alternative
 ;; to { })
 "Hello hmrld!"

 > (define bar-world 11)
 > ($$ "foo" " $bar-world, you" 12 (inc 13))
 "foo 11, you1214")

