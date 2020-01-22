;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (fixnum inc)
         (cj-env when)
         (cj-env-2 xcond)
         cj-typed
	 simple-match
	 cj-inline
	 cj-symbol
	 ;; enum  can't, circular dependency
	 (predicates function-of arguments-of)
	 (list-util let-pair)
	 named
         test)

(export (macro match-cmp)
	element? ;; XX move elsewhere, change? (scheme.scm ?) Not even used here

	cmp->equal?
	cmp->lt?
	lt->cmp

	cmp-not
	cmp-complement
	2cmp
	cmp-always-eq
	cmps->cmp
	cmps->cmp*
	(macro cmp-or)
	cmp-either-function (macro cmp-either)

	(inline @boolean-cmp
		@real-cmp
		@symbol-cmp
		@string-cmp)
	boolean-cmp
	real-cmp
	symbol-cmp
	string-cmp
	u8vector-cmp

	xserial-number ;; XX move elsewhere?
	pointer-cmp

	char-cmp
	
	length-cmp

	cmp-any
	
	german-char-downcase
	lc_perhaps-compound-1st
	lc_umlaut?
	german-string-cmp
	german-generic-cmp

	cmp-max
	cmp-min
	
	cmp-sort

	cmp-function?)

(include "cj-standarddeclares.scm")


;; (define-enum cmp
;;   eq lt gt)

(define (cmp? v)
  (case v
    ((eq lt gt) #t)
    (else #f)))

(TEST
 > (cmp? 'a)  #f
 > (cmp? 'LT) #f
 > (cmp? 'lt) #t
 > (cmp? 'gt) #t
 > (cmp? 'eq) #t)



;; A comparison operation working for all types in question

;; I'm choosing this sort order for mixed-type comparisons:
;; booleans < numbers < symbols < strings < other-types

(define (cmp:type-of v)
  (cond ((boolean? v)
	 1)
	((number? v)
	 2)
	((symbol? v)
	 3)
	((string? v)
	 4)
	(else
	 ;; generic
	 9999)))

;; universal element predicate:
(define (element? v)
  (or (boolean? v)
      (number? v)
      (symbol? v)
      (string? v)))

(define-inline (@boolean-cmp v1 v2)
  ;; #f < #t
  (cond ((eq? v1 v2)
	 'eq)
	((eq? v1 #f)
	 'lt)
	(else
	 'gt)))

(define-inline (@real-cmp v1 v2)
  (cond ((< v1 v2)
	 'lt)
	((< v2 v1)
	 'gt)
	(else
	 'eq)))

(define-inline (@symbol-cmp v1 v2)
  (cond ((eq? v1 v2)
	 'eq)
	(else
	 ;; sort by their string representation
	 (string-cmp (symbol->string v1)
		     (symbol->string v2)))))

(define-inline (@string-cmp v1 v2)
  (declare (fixnum) (not safe))
  (let ((l1 (string-length v1))
        (l2 (string-length v2)))
    (let ((l (min l1 l2)))
      (let lp ((i 0))
        (if (< i l)
            (let ((c1 (string-ref v1 i))
                  (c2 (string-ref v2 i)))
              (cond ((char<? c1 c2) 'lt)
                    ((char<? c2 c1) 'gt)
                    (else (lp (inc i)))))
            (real-cmp l1 l2))))))


;; make safe wrappers:
(insert-result-of
 (cons 'begin
       (map (lambda (typ)
	      (let ((typ? (symbol-append typ "?")))
		`(define (,(symbol-append typ "-cmp") v1 v2)
		   (define (err v)
		     (error ,(string-append "not a " typ ":") v))
		   (if (,typ? v1)
		       (if (,typ? v2)
			   (,(symbol-append "@" typ "-cmp") v1 v2)
			   (err v2))
		       (err v1)))))
	    '("boolean" "real" "symbol" "string"))))

;; ^ used to have "number", i.e. number-cmp, but can't order non-real
;; numbers


(define (u8vector-cmp v1 v2)
  ;; Gambit doesn't offer u8vector>? or similar, so..
  (let ((l1 (u8vector-length v1))
	(l2 (u8vector-length v2)))
    (let ((l (min l1 l2)))
      (let lp ((i 0))
	(if (= i l)
	    (cond ((= l1 l2)
		   'eq)
		  ((< l1 l2)
		   'lt)
		  (else
		   'gt))
	    (let ((b1 (u8vector-ref v1 i))
		  (b2 (u8vector-ref v2 i)))
	      (cond ((< b1 b2)
		     'lt)
		    ((< b2 b1)
		     'gt)
		    (else
		     (lp (inc i))))))))))
(TEST
 > (u8vector-cmp (u8vector) (u8vector))
 eq
 > (u8vector-cmp (u8vector 1) (u8vector 1))
 eq
 > (u8vector-cmp (u8vector 1) (u8vector 2))
 lt
 > (u8vector-cmp (u8vector 1 2) (u8vector 2))
 lt
 > (u8vector-cmp (u8vector 1 2 3) (u8vector 1 2))
 gt
 ;; it really looks from the beginning, should call it
 ;; |u8vector-bigendian-cmp|?  Well, lexicographic comparison,
 ;; just. Really like strings. :
 > (u8vector-cmp (u8vector 1) (u8vector 1 0))
 lt
 > (u8vector-cmp (u8vector 1 2) (u8vector 2 1))
 lt
 )

(define (generic-cmp v1 v2)
  (if (eq? v1 v2)
      'eq
      (let ((t1 (cmp:type-of v1))
	    (t2 (cmp:type-of v2)))
	(if (eq? t1 t2)
	    (case t1
	      ((1) (@boolean-cmp v1 v2))
	      ((2) (@real-cmp v1 v2))
	      ((3) (@symbol-cmp v1 v2))
	      ((4) (@string-cmp v1 v2))
	      (else
	       ;; fully generic; XXX I expect this to be slow;
	       ;; (object->string doesn't work correclty for objects
	       ;; with a serial number)
	       (u8vector-cmp (object->u8vector v1)
			     (object->u8vector v2))))
	    (xcond ((< t1 t2)
                    'lt)
                   ((< t2 t1)
                    'gt))))))


(define (xserial-number v)
  (if (##mem-allocated? v)
      (object->serial-number v)
      (error "not memory-allocated:" v)))
(define (pointer-cmp v1 v2)
  ;; use a let to force evaluation order of the arguments
  (let ((s1 (xserial-number v1)))
    (real-cmp s1
		(xserial-number v2))))

(TEST
 > (generic-cmp #f #f)
 eq
 > (generic-cmp #f #t)
 lt
 > (generic-cmp #t #f)
 gt
 > (generic-cmp 1 1)
 eq
 > (generic-cmp 1 1.)
 eq ;; well. But yeah, whatever numeric comparison thinks.
 > (generic-cmp 1 2)
 lt
 > (generic-cmp 2 0)
 gt
 > (generic-cmp 2 "Hello")
 lt
 > (generic-cmp "Hello" 2)
 gt
 > (generic-cmp "Hello" "World")
 lt
 > (generic-cmp "Hello" "abc")
 lt ;; A lt a
 > (generic-cmp "Hello" 'abc)
 gt
 > (generic-cmp 'cde 'abc)
 gt
 > (generic-cmp 'cde 'cde)
 eq
 > (generic-cmp 'b 'cde)
 lt

 ;; other types:
 > (define-type foo a)
 > (generic-cmp (make-foo 1)(make-foo 1))
 eq
 > (generic-cmp (make-foo 1)(make-foo 3))
 lt
 > (generic-cmp (make-foo 3)(make-foo 1))
 gt

 ;; a little hacky, relies on fresh instances and increasing serial numbers
 > (pointer-cmp (make-foo 1)(make-foo 1))
 lt
 > (pointer-cmp (make-foo 1)(make-foo 3))
 lt
 > (pointer-cmp (make-foo 3)(make-foo 1))
 lt
 > (define a (make-foo 1))
 > (define b (make-foo 1))
 > (pointer-cmp a b)
 lt
 > (pointer-cmp b a)
 gt
 > (pointer-cmp a a)
 eq
 > (pointer-cmp b b)
 eq
 )

(define-macro* (match-cmp v . cases)
  (let ((V (gensym 'v)))
    `(let ((,V ,v))
       (case ,V
	 ,@(append
	    (map (lambda (c)
		   (match-list*
		    c
		    ((symbol-list body0 . body)
		     (match-list*
		      symbol-list
		      ;; for proper list checking and location removal
		      (symbols
		       (for-each
                        (lambda (s)
                          (when (not (memq (source-code s) '(lt gt eq)))
                                (source-error
                                 s "expecting one of |lt|, |gt|, |eq|")))
                        symbols)
		       `(,symbols ,body0 ,@body))))))
		 cases)
	    `((else (match-cmp-error ,V))))))))

(define (match-cmp-error v)
  (error "match-cmp: no match for:" v))

(TEST
 > (match-cmp (generic-cmp 1 2) ((lt) "ha"))
 "ha"
 > (match-cmp (generic-cmp 2 1) ((lt gt) "unequal") ((eq) "equal"))
 "unequal"
 > (match-cmp (generic-cmp 2 2) ((lt gt) "unequal") ((eq) "equal"))
 "equal"
 > (with-exception-catcher
    error-exception-message
    (lambda () (match-cmp (generic-cmp 2 1) ((lt) "unequal") ((eq) "equal"))))
 "match-cmp: no match for:"
 )

;; XX move these somewhere else?

(define (cmp->equal? cmp)
  (lambda (a b)
    (match-cmp (cmp a b)
	       ((eq)
		#t)
	       ((lt gt)
		#f))))

(define (cmp->lt? cmp)
  (lambda (a b)
    (match-cmp (cmp a b)
	       ((lt)
		#t)
	       ((eq gt)
		#f))))

(define (lt->cmp lt)
  (lambda (v1 v2)
   (cond ((lt v1 v2)
	  'lt)
	 ((lt v2 v1)
	  'gt)
	 (else
	  'eq))))


(define (cmp-max cmp prefer-right? a b)
  (match-cmp (cmp a b)
	     ((lt) b)
	     ((gt) a)
	     ((eq) (if prefer-right? b a))))

(define (cmp-min cmp prefer-right? a b)
  (match-cmp (cmp a b)
	     ((lt) a)
	     ((gt) b)
	     ((eq) (if prefer-right? b a))))

(TEST
 > (cmp-min generic-cmp #t 1 3)
 1
 > (cmp-min (on car generic-cmp) #t '(1 a) '(1 b))
 (1 b)
 > (cmp-min (on car generic-cmp) #f '(1 a) '(1 b))
 (1 a)
 > (cmp-max generic-cmp #t 1 3)
 3)


;; (could be optimized slightly by changing sort)
(define (cmp-sort l cmp)
  (sort l (cmp->lt? cmp)))


(define (cmp-not v)
  (match-cmp v
	     ((eq) 'eq)
	     ((lt) 'gt)
	     ((gt) 'lt)))

(define (cmp-complement cmp)
  (lambda (a b)
    (cmp-not (cmp a b))))


;; XX naming: unlike the normal either, here the order of the
;; combinator arguments is relevant! Should this get a different name?

(define (cmp-either-function cmp1 cmp2)
  ;; run cmp2 if cmp1 gave eq (i.e. treat eq as |either| would #f)
  (lambda (a b)
    (match-cmp (cmp1 a b)
	       ((eq)
		(cmp2 a b))
	       ((lt) 'lt)
	       ((gt) 'gt))))

(define-macro* (cmp-either cmp-expr . cmp-exprs)
  ;; run cmp2 if cmp1 gave eq (i.e. treat eq as |either| would #f)
  (with-gensyms
   (a b)
   `(lambda (,a ,b)
      ,(let rec ((cmp-expr cmp-expr)
		 (cmp-exprs cmp-exprs))
	 (if (null? cmp-exprs)
	     `(,cmp-expr ,a ,b)
	     `(match-cmp (,cmp-expr ,a ,b)
			 ((eq)
			  ,(rec (car cmp-exprs) (cdr cmp-exprs)))
			 ((lt) 'lt)
			 ((gt) 'gt)))))))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#cmp-either a)
 (lambda (GEN:a-2324 GEN:b-2325) (a GEN:a-2324 GEN:b-2325))
 > (expansion#cmp-either a b c)
 (lambda (GEN:a-2326 GEN:b-2327)
   (match-cmp
    (a GEN:a-2326 GEN:b-2327)
    ((eq)
     (match-cmp
      (b GEN:a-2326 GEN:b-2327)
      ((eq) (c GEN:a-2326 GEN:b-2327))
      ((lt) 'lt)
      ((gt) 'gt)))
    ((lt) 'lt)
    ((gt) 'gt))))


(TEST
 > (define f (cmp-either-function (on car string-cmp) (on cadr real-cmp)))
 > (f '("a" 10) '("a" -2))
 gt
 > (f '("a" -10) '("a" -2))
 lt
 > (f '("b" -10) '("a" -2))
 gt
 > (f '("b" -10) '("a" 2))
 gt
 > (f '("a" 2) '("a" 2))
 eq)

;; XX use local-test.scm instead of copy-paste
(TEST
 > (define f (cmp-either (on car string-cmp) (on cadr real-cmp)))
 > (f '("a" 10) '("a" -2))
 gt
 > (f '("a" -10) '("a" -2))
 lt
 > (f '("b" -10) '("a" -2))
 gt
 > (f '("b" -10) '("a" 2))
 gt
 > (f '("a" 2) '("a" 2))
 eq)

(TEST
 > (define f (cmp-either (on car string-cmp)
			 (on cadr real-cmp)
			 (on caddr boolean-cmp)))
 > (f '("a" 10 #t) '("a" -2 #t))
 gt
 > (f '("a" -10 #t) '("a" -2 #t))
 lt
 > (f '("a" 2 #t) '("a" 2 #t))
 eq
 > (f '("a" 2 #t) '("a" 2 #f))
 gt
 > (f '("a" 2 #f) '("a" 2 #t))
 lt)



;; --- keep this?
;; turn multiple cmps into a new cmp, that compares by the cmps in
;; order of the list until one not returning eq is found
(define (2cmp cmp1 cmp2)
  (lambda (a b)
    (match-cmp (cmp1 a b)
	       ((lt) 'lt)
	       ((gt) 'gt)
	       ((eq) (cmp2 a b)))))

(define cmp-always-eq
  (lambda (a b)
    'eq))

(define (cmps->cmp cmps)
  (fold-right 2cmp
	      cmp-always-eq
	      cmps))

(define (cmps->cmp* . cmps)
  (cmps->cmp cmps))

;; (TEST
;;  )

;; --- /keep this?

(define-macro* (cmp-or . exprs)
  (if (null? exprs)
      `'eq
      `(match-cmp ,(car exprs)
		  ((eq) (cmp-or ,@(cdr exprs)))
		  ((lt) 'lt)
		  ((gt) 'gt))))

;; case-insensitive and umlaut sensitive comparison

(define (german-char-downcase c) ;; german-to-lower
  (define upper "ÄÖÜÇÉÈÀ")
  (define lower "äöüçéèà")
  (let ((len (string-length lower)))
    (let lp ((i 0))
      (if (< i len)
	  (if (char=? (string-ref upper i) c)
	      (string-ref lower i)
	      (lp (inc i)))
	  (char-downcase c)))))

(define (char-cmp a b)
  (cond ((char<? a b) 'lt)
        ((char<? b a) 'gt)
        (else 'eq)))

;; these only work for lower case (use german-char-downcase)
(define (lc_perhaps-compound-1st c)
  (case c
    ((#\ä) #\a)
    ((#\ö) #\o)
    ((#\ü) #\u)
    ((#\ç) #\c)
    ((#\é) #\e)
    ((#\è) #\e)
    ((#\à) #\a)
    (else
     c)))

;; XX ah and then not even bother to look at the second? for now?
;; anyway:
(define (lc_umlaut? c)
  (case c
    ((#\ä #\ö #\ü) #t)
    (else
     #f)))

;; fix up the strings on the fly (instead of building index containing
;; massaged strings); or, compare them on the fly? [is there any
;; difference?]
(define (german-string-cmp a b)
  (let ((lena (string-length a))
        (lenb (string-length b)))
    (let lp ((ia 0)
             (ib 0))
      (if (< ia lena)
          (if (< ib lenb)
              ;; XX ignore ö vs oe for now; treat ö same as o
              (let ((ca (lc_perhaps-compound-1st ;; for now
                         (german-char-downcase (string-ref a ia))))
                    (cb (lc_perhaps-compound-1st ;; for now
                         (german-char-downcase (string-ref b ib)))))
                (cmp-or (char-cmp ca cb)
                        (lp (inc ia)
                            (inc ib))))
              'gt)
          (if (< ib lenb)
              'lt
              'eq)))))

(TEST
 > (german-string-cmp "Hallo" "hallo")
 eq
 > (german-string-cmp "Hallo" "hallochen")
 lt
 > (german-string-cmp "Öchsel" "öchsel")
 eq
 > (german-string-cmp "Öchsel" "Ochsel")
 eq ;; hmm yeh hmm
 > (german-string-cmp "Öchsel" "Oechsel")
 lt ;; XX should not be. or?
 )

;; only 'german' for strings [for now?]
(define (german-generic-cmp a b)
  (if (and (string? a)
	   (string? b))
      (german-string-cmp a b)
      (generic-cmp a b)))



;; XX compare with lengths-= in predicates.scm, these both really should
;; be in list library right? *!*

(define (length-cmp l1 l2)
  (if (null? l1)
      (if (null? l2) 'eq 'lt)
      (if (null? l2)
	  'gt
	  (length-cmp (cdr l1) (cdr l2)))))

(TEST
 > (length-cmp '() '())
 eq
 > (length-cmp '(a) '())
 gt
 > (length-cmp '(a) '(1))
 eq
 > (length-cmp '(a) '(1 2))
 lt
 > (length-cmp '(a b) '(1 2))
 eq
 > (%try (length-cmp '(a . b) '(1 . 2)))
 (exception text: "(Argument 1) PAIR expected\n(cdr 'b)\n"))


;; also see adapted copy-paste in typed-list.scm
(define (cmp-any cmp)
  (named lp (lambda (l1 l2)
	      (if (null? l1)
		  (if (null? l2)
		      'eq
		      'lt)
		  (if (null? l2)
		      'gt
		      (let-pair
		       ((a l1*) l1)
		       (let-pair
			((b l2*) l2)
			(match-cmp (cmp a b)
				   ((eq) (lp l1* l2*))
				   ((lt) 'lt)
				   ((gt) 'gt)))))))))

(TEST
 > (def c (cmp-any real-cmp))
 > (c '() '())
 eq
 > (c '(2) '(2))
 eq
 > (c '(1) '(2))
 lt
 > (c '(2) '(1))
 gt
 > (c '(1 3) '(2 3))
 lt
 > (c '(2 3) '(1 3))
 gt
 > (c '(1) '(2 3))
 lt
 > (c '(1 3) '(2))
 lt
 > (c '(1 3) '(1 3))
 eq
 > (c '(1 4) '(1 3))
 gt
 > (c '(1 3) '(1 4))
 lt
 > (c '(1 3) '(1 3 3))
 lt
 > (c '(1 3 3) '(1 3))
 gt)



(define cmp-function? (function-of (arguments-of any? any?)
				   cmp?))

