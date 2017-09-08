;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require define-macro-star
	 test
	 (cj-env define-if-not-defined) ;; in macro expansion
	 ;; (predicates-1 list-of-length)
	 (cj-inline-1 define-inline) ;; cj-inline would give cycle
	 )

(export struct-tag?
	;; ^ XX inconsistent: struct-type etc. are all talking about
	;; 'type'; and yeah it should be that right?
	maybe-struct-tag-name
	struct-tag.name ;; stupid naming? (oo again, that isn't) (but
			;; matches, above it may not be a struct-tag
			;; and is ok)
	
	cj-struct#vector? vector?

	;; and struct-metadata:
	struct-metadata?
	make-struct-metadata-accessor
	struct-metadata
	struct-metadata.constructor-name
	@maybe-struct-tag-name
	@struct-tag?
	struct-tag-allocate!
	maybe-struct-tag->metadata
	struct-tag->metadata
	)

;;(include "cj-standarddeclares.scm")
;; no, as that would disable the modification to |vector?|
(declare (block)
	 (standard-bindings)
	 (extended-bindings))


;; A struct is a vector with an object in slot 0 that unambiguously
;; determines the struct type, implemented by using allocated,
;; non-interned objects ("tags", but they are not visual, but
;; invisibly unique; for now, sadly (no type language yet)).


;; Metadata

;; `struct-metadata` struct:

;; we can't use the struct-tag machinery yet, so hand-code the same
;; thing here for this type:
(define-if-not-defined struct-metadata:tag (list 'struct-metadata))

(define (struct-metadata? v)
  (and (##vector? v)
       (fx> (##vector-length v) 1)
       (eq? (##vector-ref v 0) struct-metadata:tag)))

(define (make-struct-metadata-accessor slot-number)
  (lambda (v)
    (if (struct-metadata? v)
	(vector-ref v slot-number)
	(error "wrong type"))))

(define (struct-metadata constructor-name)
  (vector struct-metadata:tag constructor-name))

(define struct-metadata.constructor-name (make-struct-metadata-accessor 1))


;; Mapping  from tag objects to metadata

(define-if-not-defined cj-struct:type->metadata (make-table test: eq?))

(define-inline (@maybe-struct-tag-name v)
  (declare (block)
	   (standard-bindings)
	   (extended-bindings)
	   (not safe))
  ;; @maybe-struct-tag-name is also being inlined by joo.scm in unsafe
  ;; compilation mode.
  (and (pair? v)
       (null? (cdr v))
       (let ((t (car v)))
	 (and (symbol? t)
	      t))))

(define (@struct-tag? v)
  (and (@maybe-struct-tag-name v)
       #t))


(define (struct-tag-allocate! s metadata)
  (if (symbol? s)
      (let ((t (list s)))
	(table-set! cj-struct:type->metadata t metadata)
	;; (table-set! cj-struct:lookup s t)
	t)
      (error "not a symbol:" s)))

(define (maybe-struct-tag->metadata t)
  (table-ref cj-struct:type->metadata t #f))

(define (struct-tag->metadata t)
  (or (maybe-struct-tag->metadata t)
      (error "not a struct tag:" t)))

(define (struct-tag? v)
  (and (maybe-struct-tag->metadata v)
       #t))

(define (maybe-struct-tag-name v)
  (and (struct-tag? v)
       (@maybe-struct-tag-name v)))

(define (struct-tag.name v)
  (if (struct-tag? v)
      (@maybe-struct-tag-name v)
      (error "not a struct tag:" v)))


;; 


(TEST
 > (define m (struct-metadata 'foo))
 > (define t (struct-tag-allocate! 'kkfjkif3hnunnfgw56k m))
 > (struct-tag? t)
 #t
 > (struct-tag? '(kkfjkif3hnunnfgw56k))
 #f
 > (@struct-tag? '(kkfjkif3hnunnfgw56k))
 #t
 > (equal? t '(kkfjkif3hnunnfgw56k))
 #t
 > (eq? t '(kkfjkif3hnunnfgw56k))
 #f
 > (maybe-struct-tag-name '(kkfjkif3hnunnfgw56k))
 #f
 > (@maybe-struct-tag-name '(kkfjkif3hnunnfgw56k))
 kkfjkif3hnunnfgw56k
 > (maybe-struct-tag-name t)
 kkfjkif3hnunnfgw56k
 > (@maybe-struct-tag-name t)
 kkfjkif3hnunnfgw56k

 > (eq? (struct-tag-allocate! 'kkfjkif3hnunnfgw56k m)
	(struct-tag-allocate! 'kkfjkif3hnunnfgw56k m))
 #f

 ;; cleanup, hacky
 > (table-set! cj-struct:type->metadata t)
 ;; > (table-set! cj-struct:lookup 'kkfjkif3hnunnfgw56k #f)
 )



;; Differentiate between "Scheme vectors" and those mis-used for
;; objects now:

;; (Offer in private namespace to make it accessible in standard
;; declaration mode, see in and keep in sync with
;; cj-standarddeclares.scm !)

(define (cj-struct#vector? v)
  (and (##vector? v)
       (if (fx>= (##vector-length v) 1)
	   (not (struct-tag? (##vector-ref v 0)))
	   #t)))

(set! vector? cj-struct#vector?)
