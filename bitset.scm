;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



;; A set data type holding non-negative integers up to (but excluding)
;; a predefined value

;; Implemented as bit set split into 32-bit words held in an
;; u32vector, where the first word holds the number of bits held in
;; the last slot.


(require cj-typed
	 cj-inline
	 test
	 local-test
	 test-random
	 hashcollection
	 C)

(export make-bitset
	bitset-size
	bitset-remove! (inline @bitset-remove!)
	bitset-add! (inline @bitset-add!)
	bitset-contains? (inline @bitset-contains?)
	bitset->list
	(macro bitset-declare) ;; when accessing C functions directly
	)


(include "cj-standarddeclares.scm")

;;(declare (not safe) (fixnum))


(define (bitset:i.slot i)
  (+ (arithmetic-shift i (insert-result-of (- (dec (integer-length 32))))) 1))

(define (bitset:i.bit i)
  (bitwise-and i (insert-result-of (dec 32))))

(define (bitset:i.mask i)
  (arithmetic-shift 1 (bitset:i.bit i)))

(define-typed (make-bitset [exact-natural? len]
			[boolean? initially-full?])
  (let ((v (make-u32vector (+ (bitset:i.slot (dec len)) 1)
			   (if initially-full?
			       (dec (arithmetic-shift 1 32))
			       0))))
    (u32vector-set! v 0 (inc (modulo (dec len) 32)))
    v))

(define (bitset-size s)
  (let ((len (u32vector-length s)))
    (+ (* 32 (- len 2))
       (u32vector-ref s 0))))


(define (bitset-remove! s i)
  (let ((slot (bitset:i.slot i)))
    (u32vector-set! s
		    slot
		    (bitwise-and (u32vector-ref s slot)
				 (bitwise-not (bitset:i.mask i))))
    s))

(define (bitset-add! s i)
  (let ((slot (bitset:i.slot i)))
    (u32vector-set! s
		    slot
		    (bitwise-or (u32vector-ref s slot)
				(bitset:i.mask i)))
    s))

(define (bitset-contains? s i)
  (not (zero? (bitwise-and (u32vector-ref s (bitset:i.slot i))
			   (bitset:i.mask i)))))


(defmacro (bitset-declare)
  `(c-declare "
void bitset__remove(___SCMOBJ v, int i);
void bitset__add(___SCMOBJ v, int i);
int bitset__contains(___SCMOBJ v, int i);
"))

(IF (mod:compiled?)
    (begin
      (bitset-declare)
      (c-declare "
static
unsigned int bitset__i_slot(unsigned int i) {
    return (i >> 5) + 1; // XXX use some kind of integer-length or via macro
}

static
unsigned int bitset__i_bit(unsigned int i) {
    return i & 31;  // XXX dito
}

static
___U32 bitset__i_mask(unsigned int i) {
    return 1 << bitset__i_bit(i);
}


void bitset__remove(___SCMOBJ v, int i) {
    ___U32* p= ___CAST(___U32*, ___BODY(v));
    p[ bitset__i_slot(i) ] &= (~ bitset__i_mask(i));
}


void bitset__add(___SCMOBJ v, int i) {
    ___U32* p= ___CAST(___U32*, ___BODY(v));
    p[ bitset__i_slot(i) ] |= bitset__i_mask(i);
}


int bitset__contains(___SCMOBJ v, int i) {
    ___U32* p= ___CAST(___U32*, ___BODY(v));
    return p[ bitset__i_slot(i) ] & bitset__i_mask(i);
}
")))


(IF (mod:compiled?)
    (begin
      (define (@bitset:i.slot i)
	(##c-code "___RESULT= ___FIX(bitset__i_slot(___INT(___ARG1)));" i))
      (define (@bitset:i.bit i)
	(##c-code "___RESULT= ___FIX(bitset__i_bit(___INT(___ARG1)));" i))
      (define (@bitset:i.mask i)
	(##c-code "___RESULT= ___FIX(bitset__i_mask(___INT(___ARG1)));" i))

      (TEST
       > (for-all (.. 0 500) (lambda (i) (equal? (bitset:i.slot i)
					    (@bitset:i.slot i))))
       ()
       > (for-all (.. 0 500) (lambda (i) (equal? (bitset:i.bit i)
					    (@bitset:i.bit i))))
       ()
       > (for-all (.. 0 500) (lambda (i) (equal? (bitset:i.mask i)
					    (@bitset:i.mask i))))
       ())))


(define-inline (@bitset-remove! s i)
 (IF (mod:compiled?)
     (begin
       (##c-code "bitset__remove(___ARG1, ___INT(___ARG2));"
		 s i)
       s)
     (bitset-remove! s i)))

(define-inline (@bitset-add! s i)
  (IF (mod:compiled?)
      (begin
	(##c-code "bitset__add(___ARG1, ___INT(___ARG2));"
		  s i)
	s)
      (bitset-add! s i)))

(define-inline (@bitset-contains? s i)
  (IF (mod:compiled?)
      (##c-code "___RESULT= bitset__contains(___ARG1, ___INT(___ARG2)) ?
                      ___TRU : ___FAL;"
		s i)
      (bitset-contains? s i)))


(define (bitset->list s)
  (let ((len (bitset-size s)))
    (let rec ((i 0))
      (if (< i len)
	  (if (@bitset-contains? s i)
	      (cons i (rec (+ i 1)))
	      (rec (+ i 1)))
	  '()))))


(TEST
 > (define ts
     ;; len vs. set
     '((1 #u32(1 0))
       (16 #u32(16 0))
       (32 #u32(32 0))
       (33 #u32(1 0 0))))
 > (for-all ts (applying (lambda (len set)
			   (equal? (make-bitset len #f) set))))
 () ;; no contradictions
 > (for-all ts (applying (lambda (len set)
			   (= len (bitset-size set)))))
 ())

(TEST
 > (define (test siz numentries initially-full? C?)
     (let* ((remove! (if C? @bitset-remove! bitset-remove!))
	    (add! (if C? @bitset-add! bitset-add!))
	    (contains? (if C? @bitset-contains? bitset-contains?))

	    (op! (if initially-full? remove! add!))
	    (is (make-list! numentries (C random-natural0 siz)))
	    (s (make-bitset siz initially-full?))
	    (t (.hashcollection is)))
       ;; build the set
       (for-each (C op! s _) is)
       ;; compare implementations
       (for-all (..< 0 siz)
		(lambda (i)
		  (equal? (contains? s i)
			  ((if initially-full? not identity)
			   (.contains? t i)))))))
 > (define (t initially-full? C?)
     (local-TEST*
      > (test 10 3 initially-full? C?)
      ()
      > (test 32 15 initially-full? C?)
      ()
      > (test 32 50 initially-full? C?)
      ()
      > (test 300 3 initially-full? C?)
      ()
      > (test 300 200 initially-full? C?)
      ()))

 ;; Scheme
 ;;  remove
 > (%test (t #t #f))
 ;;  add
 > (%test (t #f #f))
 ;; C
 ;;  remove
 > (%test (t #t #t))
 ;;  add
 > (%test (t #f #t)))

