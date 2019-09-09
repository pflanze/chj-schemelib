;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

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

(export sequential-pairs
        sequentialpairs->pairs ;; older, obsolete ?
        dsssl-maybe-ref ;; should move to Maybe ?
        dsssl-ref
        dsssl-delete
        dsssl-apply
        #!optional
        dsssl-delete-1)


;; also see chop in stream.scm

(def (sequential-pairs lis
                       pair-cons
                       #!optional
                       (list-cons cons)
                       (list-null '()))
     (let rec ((l lis))
       (cond ((null? l)
              list-null)
             ((pair? l)
              (let-pair
               ((k l*) l)
               (cond ((pair? l*)
                      (let-pair ((v l*) l*)
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


;; obsolete?, also see sequential-pairs
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
     (let ((alis (sequentialpairs->pairs args keyword? true/1)))
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
                     (if (null? vs)
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
 no)

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


(def (dsssl-apply fn key-args . moreargs)
     (apply fn (append key-args moreargs)))


