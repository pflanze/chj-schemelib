;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)

(export continuation-carp:maybe-continuation-outside-filename
        continuation-carp:maybe-location-outside-filename)

"Search back in continuation stack to find the first frame outside a
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

