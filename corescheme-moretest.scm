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

(TEST
 > (def optimize
        (=>* (source.corescheme
              globals: '( .>>= return * + - f or inexact? and number?
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
 (let ((x 2)) (+ x 1)) ;; XXX  apply simplification again!
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


;; can I generate programs that  are   runnable?  (only 1 data type? but ops that are  order of application sensitive; / or string-append or such)
;; cons, list  


(TEST
 > (OPTIMIZE ((lambda (x f) (f x)) f 10))
 (10 f)
 > (OPTIMIZE ((lambda (x f) (lambda (f) (f x))) f 10))
 (lambda (f) (f f)) ;; and that is WRONG, needs renaming
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
                  (if GEN:-1
                      GEN:-1
                      c)))))


;; let
(TEST
 > (ROUNDTRIP (let ((x ((lambda (z) (* z z)) 10))) x))
 (let ((x (let ((z 10)) (* z z)))) x))



(TEST
 ;; The macro expansion of:
 ;; (either (both number?
 ;;               (either inexact?
 ;;                       ...?))
 ;;         (both string?
 ;;               (=>*/1 string-length (< 500)))
 ;;         (all-of vector?
 ;;                 (=>*/1 vector-length (< 500))
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
