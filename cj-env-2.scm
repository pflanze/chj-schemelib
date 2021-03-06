;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         (cj-match mcase)
         cj-symbol
         test
         ;; ?
         C
         ;; cj-env-1 cj-source, sigh
         cj-functional
         debuggable-promise)

(export object->serial-number-string
        (macro for..<)
        (macro for..)
        (macro for..<*)
        current-unixtime*
        current-unixtime
        (macro xcase)
        (macro cond)
        (macro xcond)
        (macro pmatch)
        (macro pmatch-lambda)
        (macro string-match)
        (macro repeat)
        (macro unless)
        -e
        append-newline
        (macro future)
        future?
        @future-force
        future-force
        (macro CA))


(declare (block)(standard-bindings)(extended-bindings))
(possibly-use-debuggable-promise)



(define object->serial-number-string
  (compose number->string object->serial-number))


;; that which cj-standarddeclares.scm was.
;; no args yet, perhaps later..
;; (define-macro* (cj-declare)
;;  `(declare (block)(standard-bindings)(extended-bindings)))
;;stupid, doesn't work (macro output not looked at in right phase by compiler?)


;; stop hand-rolling these 0..n-1 loops
(define-macro* (for..< var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensyms
           (LP)
           `(##let ,LP ((,var ,from))
                   (##if (fx< ,var ,to)
                         (##begin
                          ,@body
                          (,LP (fx+ ,var 1)))))))))

(define-macro* (for.. var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensyms
           (LP)
           `(##let ,LP ((,var ,from))
                   (##if (fx<= ,var ,to)
                         (##begin
                          ,@body
                          (,LP (fx+ ,var 1)))))))))


;; same as for..< but slightly optimized
(define-macro* (for..<* var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensyms
           (LP TO)
           `(let ((,TO ,to))
              (assert (fixnum? ,TO))
              (let ,LP ((,var ,from))
                   (##if (##fixnum.< ,var ,TO)
                         (begin
                           ,@body
                           (,LP (##fixnum.+ ,var 1))))))))))

(TEST
 > (let ((v (make-vector 5))) (for..< (i 0 5) (vector-set! v i (* i i))) v)
 #(0 1 4 9 16)
 > (let ((v (make-vector 5))) (for..<* (i 0 5) (vector-set! v i (* i i))) v)
 #(0 1 4 9 16)
 )


;; move to where?
(define (current-unixtime*)
  (time->seconds (current-time)))

(define (current-unixtime)
  (inexact->exact (floor (current-unixtime*))))


(define-macro* (xcase expr . cases)
  (with-gensym
   V
   `(let ((,V ,expr))
      (case ,V
        ,@cases
        (else ;; #t doesn't work for case, only for cond
         (error "no match for:" ,V))))))

(TEST
 > (xcase 2 ((3) 'gut) ((2) 'guttoo))
 guttoo
 > (%try-error (xcase 1 ((3) 'gut) ((2) 'guttoo)))
 #(error "no match for:" 1))


(define-macro* (cond . cases)
  `(##cond ,@(map (mcase-lambda
                   ;; add *proper* local scope to enable local defines
                   (`(`cond-expr . `case-body)
                    (let ((let-of (lambda (body)
                                    ;; Empty let is not allowed, empty
                                    ;; cond is. Oh my.
                                    (if (null? body)
                                        `()
                                        `((##let () ,@body))))))
                     (if (and (pair? case-body)
                              (eq? (source-code (car case-body)) '=>))
                         `(,cond-expr => ,@(let-of (cdr case-body)))
                         `(,cond-expr ,@(let-of case-body))))))
                  cases)))

(TEST
 > (define (t v)
     (define (maybe-sqrt v)
       (if (and (number? v)
                (positive? v))
           (sqrt v)
           #f))
     (cond ((maybe-sqrt v) => maybe-sqrt)
           ((number? v) (define (sq x) (* x x)) (sq (+ v 2)))
           ((string? v))
           ;;((boolean? v) =>) invalid anyway
           (else 'no-match)))
 > (t 81)
 3
 > (t -8)
 36
 > (t "foo")
 #t ;; waiiiit, I didn't know? *Not* #!void !
 > (t 'f)
 no-match)


(define-macro* (xcond . cases)
  `(cond ,@cases
         (else (error "no match"))))


(define-macro* (pmatch expr . cases)
  (with-gensym
   V
   (let* ((have-else? #f)
          (cases* (map (mcase-lambda
                        (`(else . `rest)
                         (set! have-else? #t)
                         `(else ,@rest))
                        (`(`pexpr . `rest)
                         `(,(mcase pexpr
                                   (self-quoting?
                                    `(equal? ,V ,pexpr))
                                   (`(quote `val)
                                    `(equal? ,V ',val))
                                   (else
                                    ;; predicate
                                    `(,pexpr ,V)))
                           ,@rest)))
                       cases)))
     `(let ((,V ,expr))
        (cond ,@cases*
              ,@(if have-else? `()
                    `((else (error "no match")))))))))

(TEST
 > (define (t v)
     (pmatch v
       (123.4 'is-123_4)
       (number? 'num)
       ('foo 'is-foo)
       ("bar" 'is-bar)
       (string? 'str)))
 > (t "foo")
 str
 > (t "bar")
 is-bar
 > (t 123.4)
 is-123_4
 > (t 'foo)
 is-foo
 > (t 123)
 num
 > (%try-error (t 'bar))
 [error "no match"])

(define-macro* (pmatch-lambda . cases)
  (with-gensym
   V
   `(lambda (,V)
      (pmatch ,V
        ,@cases))))


(define-macro* (string-match expr . cases)
  (with-gensym
   V
   `(let ((,V ,expr))
      (cond ,@(map (lambda (c)
                     (mcase c
                            (`(else . `body)
                             c)
                            (`(`string-expr . `body)
                             `((string=? ,string-expr ,V)
                               ,@body))))
                   cases)))))

(TEST
 > (string-match (string-append "foo" "bar")
                 ("foobar" 1)
                 (else 'no))
 1
 > (string-match (string-append "foo" "bar")
                 ("foobarr" 1)
                 ((string-append "foob" "ar") 2)
                 (else 'no))
 2
 > (string-match (string-append "foo" "bar")
                 ((string-append "foob" "r") 2)
                 (else 'no))
 no)



;; does that really warrant a persistent name?
;; [could almost just use for..<,too?]
(define-macro* (repeat n
                       #!key
                       (init `(void))
                       (res `res)
                       #!rest
                       body)
  (if (null? body)
      (raise-source-error stx "missing body form(s)")
      (with-gensyms
       (LP C)
       `(let ,LP ((,C ,n)
                  (,res ,init))
             (if (positive? ,C)
                 (,LP (dec ,C)
                      (begin
                        ,@body))
                 ,res)))))

(TEST
 > (repeat 10 res: x init: 'foo x)
 foo
 > (repeat 2 res: x init: 2 (* x x))
 16)


;; Also see |when|. (Should this, considering that unless could be
;; understood *just* as an inverted |if|, only accept 2-argument
;; variant?)
(define-macro* (unless test . body)
  `(##if (##not ,test)
         (##begin
          ,@body)))

(define -e file-exists?)

(define (append-newline str)
  (string-append str "\n"))

(define-macro* (future expr)
  `(thread-start!
    (make-thread
     (lambda ()
       ,expr)
     'future)))

(define (future? v)
  (and (thread? v)
       (eq? (thread-name v) 'future)))
;; there's no ##thread-name, sigh

;; future-value, future-join (, future-wait)
(define @future-force thread-join!)

(define (future-force v)
  (if (future? v)
      (@future-force v)
      (error "not a future:" v)))

(TEST
 > (define f (future 12))
 > (define g (thread-start! (make-thread (lambda () 13))))
 > (future? f)
 #t
 > (future? g)
 #f
 > (@future-force f)
 12
 > (@future-force g)
 13
 > (future-force f)
 12
 > (with-exception-catcher error-exception-message (lambda () (future-force g)))
 "not a future:")



(define-macro* (CA . forms)
  (let* ((_? (lambda (v)
               (eq? (source-code v) '_)))
         (forms* (map (lambda (form)
                        (if (_? form)
                            (cons #t (gensym))
                            (cons #f form)))
                      forms))
         (vs (map cdr (filter car forms*))))
    (with-gensym
     R
     `(lambda ,(if* vs `(,@vs . ,R) R)
        (apply ,@(map cdr forms*) ,R)))))

(TEST
 > ((CA vector) 1 2)
 #(1 2)
 > ((CA vector _ '+) 1 2)
 #(1 + 2)
 > (define TEST:equal? syntax-equal?)
 > (expansion#CA vector _ '+)
 (lambda (GEN:-5527 . GEN:R-5528) (apply vector GEN:-5527 '+ GEN:R-5528))
 > (expansion#CA vector '+)
 (lambda GEN:R-5529 (apply vector '+ GEN:R-5529)))

