;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)

(export continuation-carp:maybe-continuation-outside-filename
        continuation-carp:maybe-location-outside-filename
        continuation-carp:continuations-outside-filename
        continuation-carp:locations-outside-filename)

"Search back in continuation stack to find the first frame(s) outside a
given file.

Name inspiration from Perl's Carp.pm module."


;; lib

(define (continuation-next v)
  (if (continuation? v)
      (##continuation-next v)
      (error "not a continuation:" v)))

;; duplicate; can't use gambit-sys yet due to bootstrapping (dot-oo needs us)
(define (continuation-maybe-location v)
  (if (continuation? v)
      (##continuation-locat v)
      (error "not a continuation:" v)))

;; /lib


;; (define (continuation-carp:maybe-continuation-outside-container cont container)
;;   )

;; (define (continuation-carp:maybe-location-outside-container cont container)
;;   (cont ((continuation-carp:maybe-continuation-outside-path cont container)
;;          => continuation-location)
;;         (else #f)))

;; (define-macro* (%continuation-carp:maybe-location-outside cont)
;;   (let ((container (location-container (source-location stx))))
;;     `(continuation-carp:maybe-location-outside-path cont ,container)))

;; Realizing that in dot-oo case, dot-oo--include.scm will be the
;; local one. No match.


(define (continuation-carp:maybe-continuation-outside-filename
         cont filename)
  (if (string? filename)
      (let lp ((cont cont))
        (define (next)
          (lp (continuation-next cont)))
        (and cont
             (cond
              ((continuation-maybe-location cont)
               => (lambda (loc)
                    (let ((container (location-container loc)))
                      (if (and (string? container)
                               (string=? (path-strip-directory container)
                                         filename))
                          (next)
                          cont))))
              (else
               (next)))))
      (error "need a string:" filename)))

(define (continuation-carp:maybe-location-outside-filename
         cont filename)
  (cond ((continuation-carp:maybe-continuation-outside-filename
          cont filename)
         => continuation-maybe-location)
        (else #f)))


;; Test:

;; (define (firstout fn)
;;   (let ((c (current-continuation)))
;;     (continuation-carp:maybe-location-outside-filename c fn)))
;; > (firstout "continuation-carp.scm")
;; [(console) 52]
;; > (firstout "foo")
;; ["....lib/continuation-carp.scm" 720974]



;; ugly partial COPY-PASTE


(define (continuation-carp:continuations-outside-filename
         cont filename n-levels)
  (if (string? filename)
      (let rec ((cont cont)
                (n n-levels))
        (define (next n)
          (rec (continuation-next cont) n))
        (if (and cont (positive? n))
            (cond
             ((continuation-maybe-location cont)
              => (lambda (loc)
                   (let ((container (location-container loc)))
                     (if (and (string? container)
                              (string=? (path-strip-directory container)
                                        filename))
                         (next n)
                         (cons cont
                               (next (dec n)))))))
             (else
              ;; keep n up?? 
              (next n)))
            '()))
      (error "need a string:" filename)))

(define (continuation-carp:locations-outside-filename
         cont filename n-levels)
  (map continuation-maybe-location
       (continuation-carp:continuations-outside-filename
        cont filename n-levels)))


;; Test:

;; (define (firstout2 fn n)
;;   (let ((c (current-continuation)))
;;     (continuation-carp:locations-outside-filename c fn n)))
;; > (firstout2 "foo" 1)
;; ([".../lib/continuation-carp.scm" 721026])
;; > (firstout2 "foo" 2)
;; ([".../lib/continuation-carp.scm" 721026]
;;  [".../lib/continuation-carp.scm" 131202])
;; > (firstout2 "foo" 3)
;; ([".../lib/continuation-carp.scm" 721026]
;;  [".../lib/continuation-carp.scm" 131202]
;;  [(console) 36])
;; > (firstout2 "foo" 4)
;; ([".../lib/continuation-carp.scm" 721026]
;;  [".../lib/continuation-carp.scm" 131202]
;;  [(console) 37])
;; > (firstout2 "foo" 0)
;; ()
;; > (firstout2 "continuation-carp.scm" 2)
;; ([(console) 40])

