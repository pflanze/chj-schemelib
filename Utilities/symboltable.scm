;;; Copyright 2010 by Christian Jaeger <chrjae@gmail.com>

;;; This file is part of GIT System.
;;;
;;;    GIT System is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU Lesser General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    GIT System is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU Lesser General Public License for more details.
;;;
;;;    You should have received a copy of the GNU Lesser General Public License
;;;    along with GIT System.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;;; Identifiers for tables and their fields
;;;

(c-declare "
#define likely(expr) __builtin_expect(expr, 1)
#define unlikely(expr) __builtin_expect(expr, 0)
")

(define-struct persistentidentifier
  constructor-name: _make-persistentidentifier
  id
  maybe-parent
  name)

;; XX: keep in sync with above definition!
(c-declare "
___SCMOBJ ___persistentidentifier_symbol= 0;
static
int ___persistentidentifierp(___SCMOBJ x)
{
    ___WORD ___temp;
    return likely(___VECTORP(x))
	    && likely(___VECTORLENGTH(x)>=___FIX(1))
	    && likely(___VECTORREF(x,___FIX(0)) == ___persistentidentifier_symbol);
}
")
(##c-code "___persistentidentifier_symbol=___ARG1;" 'persistentidentifier)
(define (persistentidentifier?:c x)
  (##c-code "___RESULT= ___persistentidentifierp(___ARG1) ? ___TRU : ___FAL;" x))

(define (make-persistentidentifier id maybe-parent name)
  (values (inc id)
	  (_make-persistentidentifier id maybe-parent name)))

(define persistentidentifier-eq?
  (on persistentidentifier-id fx=))

(define (persistentidentifier-id-eq? id pi)
  (fx= id (persistentidentifier-id pi)))

(TEST
 > (persistentidentifier-eq? '#(persistentidentifier 12 #f "a")
			     '#(persistentidentifier 11 #f "b"))
 #f
 > (persistentidentifier-eq? '#(persistentidentifier 12 #f "a")
			     '#(persistentidentifier 12 #f "a"))
 #t 
 ;; This should of course never happen:
 > (persistentidentifier-eq? '#(persistentidentifier 12 #f "a")
			     '#(persistentidentifier 12 #f "b"))
 #t 
 )

(define (strings->persistentidentifiers maybe-parent)
  (lambda (from-id lis)
    (fold-right
     (lambda-values
      (str (id pis))
      (letv ((id* pi) (make-persistentidentifier id maybe-parent str))
	    (values id*
		    (cons pi pis))))
     (values from-id
	     '())
     lis)))

(TEST
 > (define-values (id l) ((strings->persistentidentifiers #f) 10 '("a" "b" "ha")))
 > id
 13
 > l
 (#(persistentidentifier 12 #f "a")
   #(persistentidentifier 11 #f "b")
   #(persistentidentifier 10 #f "ha"))
 )


;; ==========================================================================
;;;
;;;; Hash tables with persistentidentifier as keys
;;;

(define pitable? vector?)
;; XX: keep in sync with above definition!
(c-declare "
static
int ___pitablep(___SCMOBJ x)
{
    ___WORD ___temp;
    return likely(___VECTORP(x));
}
")

(define empty-pitable (vector))

;; "size" here meaning the number of slots in the vector (pairs of
;; vector entries)

(define (pitable-length->pitable-size^ len)
  (fxlength (+ len (fxarithmetic-shift len -2))))

(define (pitable-size^->vector-length size)
  (fxarithmetic-shift 1 (inc size)))
;; ^-inverse-v (for allowed values)
(define (vector-length->pitable-size^ vlen)
  (fx- (fxlength vlen) 2))

(define pitable-length->vector-length
  (compose pitable-size^->vector-length
	   pitable-length->pitable-size^))

(TEST
 > (pitable-length->vector-length 0)
 2
 > (pitable-length->vector-length 1)
 4
 > (pitable-length->vector-length 2)
 8
 > (pitable-length->vector-length 3)
 8
 > (pitable-length->vector-length 4)
 16
 > (pitable-length->vector-length 6)
 16
 > (pitable-length->vector-length 7)
 32
 > (pitable-length->vector-length 12)
 32
 > (pitable-length->vector-length 13)
 64
 )

;; (define (pitable-vec->))

(define (inc2 n)
  (fx+ n 2))

(define (inc2/top n top)
  (let ((n (inc2 n)))
    (if (fx>= n top)
	0
	n)))

(define pitable-vector-length vector-length)

(define (pitable-size^->mask size^)
  (dec (fxarithmetic-shift 1 size^)))

(define _pitable-update
  (lambda (t key handle-found handle-not-found)
    (let* ((vlen (pitable-vector-length t))
	   (vec t)
	   (id (persistentidentifier-id key))
	   (slot (bitwise-and
		  id
		  (pitable-size^->mask (vector-length->pitable-size^ vlen)))))
    ;;(warn "vlen,slot=" vlen slot)
      (let lp ((i (fxarithmetic-shift slot 1)))
      ;;(warn "   lp: i,key2=" i (vector-ref vec i))
	(cond ((vector-ref vec i)
	       => (lambda (key*)
		    (if (persistentidentifier-id-eq? id key*)
			(handle-found vec (inc i))
			(lp (inc2/top i vlen)))))
	      (else
	       (handle-not-found)))))))

(define (pitable-ref:scheme t key alternate-value)
  (_pitable-update t key vector-ref (lambda ()
				      alternate-value)))

(c-declare "
// #include <stdio.h>

static inline
___WORD ___fxlength(___WORD x)
{
#if 0
   // use (supposedly portable) Gambit macro
   ___WORD ___temp;
   return ___INT(___FIXLENGTH(___FIX(x)));
#else
   // use faster gcc builtin
   // ATTENTION: gcc info doc says __builtin_clz* is undefined for 0;
   // \"for me\" this gives 0, as the Gambit function does.
   return
# if ___WORD_WIDTH == 32
       32 - __builtin_clzl
# else
#  if ___WORD_WIDTH == 64
       64 - __builtin_clzll
#  else
#    error \"unknown ___WORD_WIDTH\"
#  endif
# endif
          (x);
#endif
}

___SCMOBJ ___error_invalid_type= 0;
")

(define pitable-ref:c:invalid-type (gensym))
(##c-code "___error_invalid_type=___ARG1;" pitable-ref:c:invalid-type)
(define (pitable-ref:c t key alternate-value)
  (declare (standard-bindings)
	   (extended-bindings)
	   (not safe))
  (let ((res (##c-code "
___SCMOBJ t = ___ARG1;
___SCMOBJ key = ___ARG2;
___SCMOBJ alternate_value = ___ARG3;

#define ___persistentidentifier_id(pi) ___INT(___VECTORREF(pi,___FIX(1)))

if (unlikely(! ___pitablep(t))) {
    ___RESULT= ___error_invalid_type;
    goto end;
}

___SCMOBJ vec = t;
___WORD vlen = ___INT(___VECTORLENGTH(vec));

if (unlikely(! ___persistentidentifier_id(key))) {
    ___RESULT= ___error_invalid_type;
    goto end;
}

___WORD id = ___persistentidentifier_id(key);
// (define-inline (vector-length->pitable-size^ vlen)
//    (fx- (fxlength vlen) 2))
___WORD sizepot = ___fxlength(vlen)  - 2;
// (define-inline (pitable-size^->mask size^)
//   (dec (fxarithmetic-shift 1 size^)))
___WORD mask = (1 << sizepot)-1;
___WORD slot = id & mask;

// printf(\"vlen=%ld, id=%ld, sizepot=%ld, mask=%ld, slot=%ld\\n\", vlen,id,sizepot,mask,slot);

___WORD i= slot << 1;
lp: {
    ___SCMOBJ key2 = ___VECTORREF(vec,___FIX(i));
    // printf(\"   lp:  i=%ld, key2=%ld\\n\", i,key2);
    if (unlikely (key2 == ___FAL)) {
        ___RESULT= alternate_value;
    } else {
	if (unlikely(! ___persistentidentifierp(key2))) {
	    ___RESULT= ___error_invalid_type;
	    goto end;
	}
        if (___persistentidentifier_id(key2) == id) {
            ___RESULT= ___VECTORREF(vec,___FIX(i+1));
        } else {
            i=i+2;
            if (i >= vlen) i=0;
            goto lp;
        }
    }
}
end:
"
		       t key alternate-value)))
    (if (eq? res pitable-ref:c:invalid-type)
	(error "invalid type")
	res)))

(define pitable-ref pitable-ref:c) ;; choose


(define (pitable-update! t key fn #!optional not-found)
  (_pitable-update t key
		   (lambda (vec i)
		     (vector-set! vec i (fn (vector-ref vec i))))
		   (or not-found
		       (lambda ()
			 (error "key not found")))))

(define (pitable-update t key fn #!optional not-found)
  (_pitable-update t key
		   (lambda (vec i)
		     (let ((vec (vector-copy vec)))
		       (vector-set! vec i (fn (vector-ref vec i)))
		       vec))
		   (or not-found
		       (lambda ()
			 (error "key not found")))))

(define (list->pitable l)
  (let* ((len (length l))
	 (size^ (pitable-length->pitable-size^ len))
	 (vlen (pitable-size^->vector-length size^))
	 (vec (make-vector vlen #f))
	 (mask (pitable-size^->mask size^)))
    (let lp ((l l))
      (if (not (null? l))
	  (let* ((a (car l))
		 ;; ch carl.  was war das  wo w  schonwd ?
		 ;;no fxbitwise-and ?...
		 (slot (bitwise-and (persistentidentifier-id (car a)) mask)))
	    (let lp ((i (fxarithmetic-shift slot 1)))
	      (if (vector-ref vec i)
		  (lp (inc2/top i vlen))
		  (begin
		    (vector-set! vec i (car a))
		    (vector-set! vec (inc i) (cdr a)))))
	    (lp (cdr l)))))
    vec))

(TEST
 > (define t
     (list->pitable (map (lambda (pi)
			   (cons pi
				 (string-append "moo-"
						(persistentidentifier-name pi))))
			 l)))
 > t
 #(#(persistentidentifier 12 #f "a")
    "moo-a"
    #f
    #f
    #(persistentidentifier 10 #f "ha")
    "moo-ha"
    #(persistentidentifier 11 #f "b")
    "moo-b")
 > (pitable-ref t '#(persistentidentifier 10 #f "ha") 'not-found)
 "moo-ha"
 > (pitable-ref t '#(persistentidentifier 13 #f "hu") 'not-found)
 not-found
 > (pitable-update! t '#(persistentidentifier 10 #f "ha") (lambda (x) 1))
 > (pitable-ref t '#(persistentidentifier 10 #f "ha") 'not-found)
 1
 > (pitable-update! t '#(persistentidentifier 10 #f "ha") inc)
 > (pitable-ref t '#(persistentidentifier 10 #f "ha") 'not-found)
 2
 > (define t2 (pitable-update t '#(persistentidentifier 10 #f "ha") inc))
 > (pitable-ref t '#(persistentidentifier 10 #f "ha") 'not-found)
 2
 > (pitable-ref t2 '#(persistentidentifier 10 #f "ha") 'not-found)
 3
 > (pitable-update t '#(persistentidentifier 13 #f "hu") inc (lambda () 'no))
 no

 )
