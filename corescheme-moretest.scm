;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         corescheme
         corescheme-to-scheme
         corescheme-optimize
         test)

"Some random additional tests for the corescheme libraries."

;; (run-tests "lib/corescheme" "lib/corescheme-to-scheme" "lib/corescheme-optimize" "lib/corescheme-moretest")

(TEST
 > (def optimize
        (=>* (source.corescheme
              globals: '( .>>= return * + - f inexact? number?
                               string-length < string? vector-length
                               vector? ...?))
             corescheme-optimize
             corescheme-to-scheme))

 > (defmacro (OPTIMIZE* expr)
     `(optimize (quote-source ,expr)))

 > (defmacro (OPTIMIZE expr)
     `(cj-desourcify (optimize (quote-source ,expr)))))


(TEST
 > (OPTIMIZE (lambda (x) (f x)))
 f
 > (OPTIMIZE (lambda (x) ((f f) x)))
 ;; because (f f) would run in different dynamic context
 (lambda (x) ((f f) x))
 > (OPTIMIZE (lambda (x) (x f)))
 (lambda (x) (x f))
 > (OPTIMIZE (let ((x 100)) (+ x x)))
 (let ((x 100)) (+ x x))
 > (OPTIMIZE (let ((x 100)) (+ x 10)))
 (+ 100 10)
 > (OPTIMIZE (let ((x 100) (y 120)) (f (* x 10))))
 (f (* 100 10))
 > (OPTIMIZE (let ((x 100) (y 120)) (f (* x 10 y))))
 (f (* 100 10 120))
 > (OPTIMIZE (let ((x 100) (y 120)) (f (* x 10 y y))))
 (let ((x 100) (y 120)) (f (* x 10 y y)))
 ;; so, now want to optimize it partially, ok?
 )


(TEST
 > (OPTIMIZE (let ((x f)) 1))
 1
 > (OPTIMIZE (let ((x f)) x))
 f
 > (OPTIMIZE (let* ((x f) (y x)) x))
 f
 > (OPTIMIZE (let* ((x f) (y x)) y))
 f
 > (OPTIMIZE (let ((let 1)) let))
 1
 > (OPTIMIZE (let ((let 1)) (let)))
 (1)
 > (with-exception-catcher source-error-message
                           (& (OPTIMIZE (let* ((let 1) (lambda let)) (lambda () 2)))))
 "unquoted empty list treated as invalid function application"
 > (OPTIMIZE (let* ((let 1) (lambda let)) (lambda (quote x) 2)))
 (1 'x 2)
 > ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2)
 3
 > (OPTIMIZE ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2))
 (let ((x 2)) (+ x 1)) ;; XX  apply simplification again!
 > (optimize (OPTIMIZE ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2)))
 (+ 2 1) ;; and then, constant folding hehe ?

 > (OPTIMIZE (let* ((let 1) (lambda let)) (lambda (f) 2)))
 (1 (f) 2)
 > (OPTIMIZE (let* ((let 1) (lambda let) (f 3)) (lambda (f) 2)))
 (1 (3) 2)
 > (OPTIMIZE f)
 f
 > (OPTIMIZE 'f)
 'f
 > (OPTIMIZE ''f)
 ''f
 > (OPTIMIZE '''f)
 '''f
 )


(TEST
 > (OPTIMIZE ((lambda (x f) (f x)) f 10))
 (10 f)
 > (OPTIMIZE ((lambda (x f) (lambda (f) (f x))) f 10))
 (lambda (f) (f f)) ;; XXX WRONG, needs renaming
 )

(TEST
 > (define TEST:equal? syntax-equal?)
 > (ROUNDTRIP (lambda (a b c) (and a b c)))
 (lambda (a b c) (and a b c))
 > (ROUNDTRIP (lambda (d e) (##and d e)))
 (lambda (d e) (and d e))
 > (ROUNDTRIP (lambda (a b c)
                (and a (or b c))))
 (lambda (a b c) (and a
                (let ((GEN:-1 b))
                  (or GEN:-1
                      c)))))


;; let
(TEST
 > (ROUNDTRIP (let ((x ((lambda (z) (* z z)) 10))) x))
 (let ((x (let ((z 10)) (* z z)))) x)
 > (OPTIMIZE (let ((x ((lambda (z) (* z z)) 10))) x))
 (let ((z 10)) (* z z)))

;; |and|, |or|, |begin|
(TEST
 > (define TEST:equal? syntax-equal?)
 > (ROUNDTRIP (lambda (a b c d e) (and (and a (and b c d) e))))
 (lambda (a b c d e) (and a b c d e))
 > (ROUNDTRIP (lambda (a b c d e) (and (or a (and b c d) e))))
 (lambda (a b c d e)
   (let ((GEN:V-6063 a))
     (or GEN:V-6063
         (let ((GEN:V-6062 (and b (and c d)))) (or GEN:V-6062 e)))))
 ;;XXX also why was nested |and| not flattened
 > (ROUNDTRIP (lambda (a b c d e) (and (or a (or b c d) e))))
 (lambda (a b c d e)
   (let ((GEN:V-6065 a))
     (or GEN:V-6065
         (let ((GEN:V-6064
                (let ((GEN:V-6067 b))
                  (or GEN:V-6067
                      (let ((GEN:V-6066 c)) (or GEN:V-6066 d))))))
           (or GEN:V-6064 e)))))
 ;; XX or
 > (ROUNDTRIP (lambda (a b c d e) (and a)))
 (lambda (a b c d e) a)
 > (ROUNDTRIP (lambda (a b c d e) (and (begin (a) (begin (b) (c) (d)) e))))
 (lambda (a b c d e) (a) (begin (b) (c) (d)) e)
 ;; ^ because optimization other than on corescheme-extended is not
 ;; part of roundtrip
 > (OPTIMIZE (lambda (a b c d e) (and (begin (a) (begin (b) (c) (d)) e))))
 (lambda (a b c d e) (a) (b) (c) (d) e)
 > (OPTIMIZE (lambda (a b c d e) (and (begin a (begin b c d) e))))
 (lambda (a b c d e) a b c d e) ;; XX optimize away unused constants

 ;; fun, optimizer can re-use existing variables in scope:
 > (ROUNDTRIP (lambda (a b c d e) (or a b)))
 (lambda (a b c d e) (let ((GEN:V-8915 a)) (or GEN:V-8915 b)))
 > (OPTIMIZE (lambda (a b c d e) (or a b)))
 (lambda (a b c d e) (or a b))
 ;; ^ although, upcoming |or|-prettyfier will undo this again.
 > (OPTIMIZE (lambda (a b c d e) (or (+ a c) b)))
 (lambda (a b c d e) (let ((GEN:V-8917 (+ a c))) (or GEN:V-8917 b)))
 )


(TEST
 ;; The macro expansion of:
 ;; (either (both number?
 ;;               (either inexact?
 ;;                       ...?))
 ;;         (both string?
 ;;               (=>* string-length (< 500)))
 ;;         (all-of vector?
 ;;                 (=>* vector-length (< 500))
 ;;                 ...?))
 > (repeatedly
    3 optimize
    (OPTIMIZE
     (let ((GEN:-8906
            (let ((GEN:-8910
                   (let ((GEN:-8912 ...?))
                     (lambda (GEN:V-8913)
                       (or (inexact? GEN:V-8913)
                           (GEN:-8912 GEN:V-8913))))))
              (lambda (GEN:V-8911)
                (and (number? GEN:V-8911)
                     (GEN:-8910 GEN:V-8911)))))
           (GEN:-8907
            (let ((GEN:-8914
                   (lambda (GEN:V-8916)
                     (let ((GEN:tmp-8917 (string-length GEN:V-8916)))
                       (let ((GEN:tmp-8918 (< GEN:tmp-8917 500)))
                         GEN:tmp-8918)))))
              (lambda (GEN:V-8915)
                (and (string? GEN:V-8915)
                     (GEN:-8914 GEN:V-8915)))))
           (GEN:-8908
            (let ((GEN:-8919
                   (lambda (GEN:V-8922)
                     (let ((GEN:tmp-8923 (vector-length GEN:V-8922)))
                       (let ((GEN:tmp-8924 (< GEN:tmp-8923 500)))
                         GEN:tmp-8924))))
                  (GEN:-8920 ...?))
              (lambda (GEN:V-8921)
                (and (vector? GEN:V-8921)
                     (GEN:-8919 GEN:V-8921)
                     (GEN:-8920 GEN:V-8921))))))
       (lambda (GEN:V-8909)
         (or (GEN:-8906 GEN:V-8909)
             (GEN:-8907 GEN:V-8909)
             (GEN:-8908 GEN:V-8909))))))

 (lambda (GEN:V-8909)
   (or (and (number? GEN:V-8909) (or (inexact? GEN:V-8909) (...? GEN:V-8909)))
       (and (string? GEN:V-8909)
            (< (string-length GEN:V-8909) 500))
       (and (vector? GEN:V-8909)
            (< (vector-length GEN:V-8909) 500)
            (...? GEN:V-8909))))
 
 )
