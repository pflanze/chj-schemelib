;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XXX shouldn't this be merged with cj-source-lambda.scm ?

(require easy-1
         cj-alist
         (list-util let-pair)
         (improper-list improper-fold)
         test)

(export dsssl?
        sequential-pairs
        sequentialpairs->pairs ;; older, obsolete ?
        dsssl->alist
        alist->dsssl
        dsssl-maybe-ref ;; should move to Maybe ?
        dsssl-ref
        dsssl-delete
        dsssl-defaults
        dsssl-apply
        #!optional
        dsssl-delete-1)

(include "cj-standarddeclares.scm")


(define (dsssl? v)
  (cond ((null? v) #t)
        ((pair? v) (let-pair ((a r) v)
                             (and (keyword? a)
                                  (pair? r)
                                  (dsssl? (cdr r)))))
        (else #f)))

(TEST
 > (dsssl? '())
 #t
 > (dsssl? '(foo: 1))
 #t
 > (dsssl? '(foo:))
 #f
 > (dsssl? '(foo: 2 bar: "fo"))
 #t
 > (dsssl? '(foo: 2 bar:))
 #f
 > (dsssl? '(foo: 2 "fo" bar:))
 #f)


;; also see chop in stream.scm

(def (sequential-pairs lis
                       pair-cons
                       #!optional
                       (list-cons cons)
                       (list-null '())
                       (assert-key! (lambda (v) #!void))
                       (assert-value! (lambda (v) #!void)))
     (let rec ((l lis))
       (cond ((null? l)
              list-null)
             ((pair? l)
              (let-pair
               ((k l*) l)
               (cond ((pair? l*)
                      (let-pair ((v l*) l*)
                                (assert-key! k)
                                (assert-value! v)
                                (list-cons (pair-cons k v)
                                           (rec l*))))
                     ((null? l*)
                      (error (string-append
                              "uneven number of elements where"
                              " sequential pairs are expected:")
                             lis))
                     (else
                      (error "improper list:" lis)))))
             (else
              (error "improper list:" lis)))))

(TEST
 > (sequential-pairs '(a 1 b 2) cons)
 ((a . 1) (b . 2))
 > (sequential-pairs '(a 1) vector)
 (#(a 1))
 > (%try-error (sequential-pairs '(a 1 2) cons))
 #(error
   "uneven number of elements where sequential pairs are expected:"
   (a 1 2))
 > (%try-error (sequential-pairs '(a 1 . 2) cons))
 #(error "improper list:" (a 1 . 2))
 > (%try-error (sequential-pairs '(a . 1) cons))
 #(error "improper list:" (a . 1)))


(def (dsssl->alist l)
     (continuation-capture
      (lambda (exit)
        (let ((asserter
               (lambda (pred typestr)
                 (lambda (v)
                   (or (pred v)
                       (continuation-graft
                        exit
                        error (string-append "expecting " typestr ", got:") v))))))
          (sequential-pairs
           l cons cons '()
           (asserter keyword? "keyword"))))))

(TEST
 > (%try (dsssl->alist '(a 1)))
 (exception text: "expecting keyword, got: a\n")
 > (dsssl->alist '(a: 1 b: 2))
 ((a: . 1) (b: . 2)))


(def (alist->dsssl l)
     (if (null? l)
         '()
         (let-pair ((a l*) l)
                   (let-pair ((k v) a)
                             (cons k
                                   (cons v
                                         (alist->dsssl l*)))))))

(TEST
 > (alist->dsssl '((a: . 1) (b: . 2)))
 (a: 1 b: 2))



;; obsolete?, also see sequential-pairs, and chop/map in stream.scm
(define (sequentialpairs->pairs lis
                                #!optional
                                key-type?
                                value-type?)
  (let rec ((lis lis))
    (if (null? lis)
        '()
        (let ((key (car lis))
              (val (cadr lis)))
          (when key-type? (assert (key-type? key)))
          (when value-type? (assert (value-type? val)))
          (cons (cons key val)
                (rec (cddr lis)))))))

(TEST
 > (sequentialpairs->pairs '(a 1) symbol? number?)
 ((a . 1)))



;; 'restargs-keyref' 'keyargs-maybe-ref'
(def (dsssl-maybe-ref args #(keyword? key))
     (let ((alis (dsssl->alist args)))
       (eq-alist-maybe-ref alis key)))

(TEST
 > (dsssl-maybe-ref '(a: 1) a:)
 1
 > (dsssl-maybe-ref '(a: 1) b:)
 #f)


(def (dsssl-ref args #(keyword? key) alternative)
     (let lp ((vs args))
       (if (null? vs)
           alternative
           (let-pair ((k vs*) vs)
                     (if (null? vs*)
                         (error "uneven argument count in:" args)
                         (let-pair ((v vs**) vs*)
                                   (if (eq? key k)
                                       v
                                       (lp vs**))))))))

(TEST
 > (def vs '(a: 1 b: 2 b: 3 c: 4))
 > (dsssl-ref vs b: 'no)
 2
 > (dsssl-ref vs x: 'no)
 no
 > (%try (dsssl-ref '(a: 2 b:) x: 'no))
 (exception text: "uneven argument count in: (a: 2 b:)\n"))


;; and since the above was not enough..
(TEST
 > (dsssl-ref '(a: #f b: #f c: #t) a: 'no)
 #f
 > (dsssl-ref '(a: #f b: #f c: #t) b: 'no)
 #f
 > (dsssl-ref '(a: #f b: #f c: #t) c: 'no)
 #t
 > (dsssl-ref '(a: #f b: #f c: #t) d: 'no)
 no)


(def (dsssl-delete-1 args #(keyword? key))
     (let rec ((vs args))
       (if (null? vs)
           vs
           (let-pair ((k vs*) vs)
                     (if (null? vs)
                         (error "uneven argument count in:" args)
                         (let-pair ((v vs**) vs*)
                                   (if (eq? key k)
                                       (rec vs**)
                                       (cons* k v (rec vs**)))))))))

(def (dsssl-delete args keyS)
     ;; OK this could be optimized (pass a (function (keyword?)
     ;; boolean?), in a single sweep, although that one would need a
     ;; more efficient data structure than a list)
     (improper-fold (flip dsssl-delete-1)
                    args
                    keyS))

(TEST
 > (def vs '(a: 1 b: 2 b: 3 c: 4))
 > (dsssl-delete vs x:)
 (a: 1 b: 2 b: 3 c: 4)
 > (dsssl-delete vs c:)
 (a: 1 b: 2 b: 3)
 > (dsssl-delete vs a:)
 (b: 2 b: 3 c: 4)
 > (dsssl-delete vs b:)
 (a: 1 c: 4)
 > (dsssl-delete vs '(a: b:))
 (c: 4))


(def (dsssl-defaults args args-defaults)
     (let ((args* (dsssl->alist args))
           (args-defaults* (dsssl->alist args-defaults)))
       (alist->dsssl
        (fold (lambda (k+v res)
                (let ((k (car k+v)))
                  (cond ((assq k res) ;; <- or check args* ?
                         => (lambda-pair ((k* v*))
                                    (if v*
                                        res
                                        (eq-alist-replace res k+v))))
                        (else
                         (cons k+v res)))))
              args*
              args-defaults*))))

(TEST
 > (def t (=>* (dsssl-delete '(num-buckets:))
               (dsssl-defaults '(oversampling: 10))))
 > (t '(foo: 1 num-buckets: #f oversampling: 30))
 (foo: 1 oversampling: 30)
 > (t '(foo: 1 num-buckets: 10 oversampling: #f baz: #f))
 (foo: 1 oversampling: 10 baz: #f)
 > (t '(foo: 1 baz: #f))
 (oversampling: 10 foo: 1 baz: #f))



(def (dsssl-apply fn key-args . moreargs)
     (apply fn (append key-args moreargs)))


