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
;;;; Hash tables with symbols as keys
;;;

(c-declare "
#define likely(expr) __builtin_expect(expr, 1)
#define unlikely(expr) __builtin_expect(expr, 0)
")

(c-declare "
#define ___keyp ___SYMBOLP
")
(define (symboltable:key? x)
  (##c-code "___RESULT= ___keyp(___ARG1) ? ___TRU : ___FAL;" x))

(define symboltable:key-eq?
  eq?)

(define (symboltable:key-id v)
  (if (symboltable:key? v)
      (##symbol-hash v)
      (error "invalid type")))

(define symboltable:key-name symbol->string)

(define symboltable? vector?)
;; XX: keep in sync with above definition!
(c-declare "
static inline
int ___tablep(___SCMOBJ x)
{
    ___WORD ___temp;
    return ___VECTORP(x);
}
")

(define empty-symboltable (vector))

;; "size" here meaning the number of slots in the vector (pairs of
;; vector entries)

(define (symboltable:length->size^ len)
  (fxlength (+ len (fxarithmetic-shift len -2))))

(define (symboltable:size^->vector-length size)
  (fxarithmetic-shift 1 (inc size)))
;; ^-inverse-v (for allowed values)
(define (symboltable:vector-length->size^ vlen)
  (fx- (fxlength vlen) 2))

(define symboltable:length->vector-length
  (compose symboltable:size^->vector-length
	   symboltable:length->size^))

(TEST
 > (symboltable:length->vector-length 0)
 2
 > (symboltable:length->vector-length 1)
 4
 > (symboltable:length->vector-length 2)
 8
 > (symboltable:length->vector-length 3)
 8
 > (symboltable:length->vector-length 4)
 16
 > (symboltable:length->vector-length 6)
 16
 > (symboltable:length->vector-length 7)
 32
 > (symboltable:length->vector-length 12)
 32
 > (symboltable:length->vector-length 13)
 64
 )

;; COPIES/DOUBLES
(define (inc2 n)
  (fx+ n 2))

(define (inc2/top n top)
  (let ((n (inc2 n)))
    (if (fx>= n top)
	0
	n)))
;;/ DOUBLES

(define symboltable:vector-length vector-length)

(define (symboltable:size^->mask size^)
  (dec (fxarithmetic-shift 1 size^)))

(define symboltable:_update
  (lambda (t key handle-found handle-not-found)
    (let* ((vlen (symboltable:vector-length t))
	   (vec t)
	   (id (symboltable:key-id key))
	   (slot (bitwise-and
		  id
		  (symboltable:size^->mask (symboltable:vector-length->size^ vlen)))))
      ;;(warn "vlen,slot=" vlen slot)
      (let lp ((i (fxarithmetic-shift slot 1)))
	;;(warn "   lp: i,key2=" i (vector-ref vec i))
	(cond ((vector-ref vec i)
	       => (lambda (key*)
		    (if (symboltable:key-eq? key* key)
			(handle-found vec (inc i))
			(lp (inc2/top i vlen)))))
	      (else
	       (handle-not-found)))))))

(define (symboltable-ref:scheme t key alternate-value)
  (symboltable:_update t key vector-ref (lambda ()
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

(define symboltable-ref:c:invalid-type (gensym))
(##c-code "___error_invalid_type=___ARG1;" symboltable-ref:c:invalid-type)
(define (symboltable-ref:c t key alternate-value)
  (declare (standard-bindings)
	   (extended-bindings)
	   (not safe))
  (let ((res (##c-code "
___SCMOBJ t = ___ARG1;
___SCMOBJ key = ___ARG2;
___SCMOBJ alternate_value = ___ARG3;

#define ___key_id(v) ___INT(___VECTORREF(v,___FIX(1)))
// lol same

if (unlikely(! ___tablep(t))) {
    ___RESULT= ___error_invalid_type;
    goto end;
}

___SCMOBJ vec = t;
___WORD vlen = ___INT(___VECTORLENGTH(vec));

if (unlikely(! ___key_id(key))) {
    ___RESULT= ___error_invalid_type;
    goto end;
}

___WORD id = ___key_id(key);
// (define-inline (symboltable:vector-length->size^ vlen)
//    (fx- (fxlength vlen) 2))
___WORD sizepot = ___fxlength(vlen)  - 2;
// (define-inline (symboltable:size^->mask size^)
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
/*
	if (unlikely(! ___keyp(key2))) {
	    ___RESULT= ___error_invalid_type;
	    goto end;
	}
*/
        if (key2 == key) { // XX only works for interned symbols!
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
    (if (eq? res symboltable-ref:c:invalid-type)
	(error "invalid type")
	res)))

(define symboltable-ref symboltable-ref:c) ;; choose


(define (symboltable-update! t key fn #!optional not-found)
  (symboltable:_update t key
		   (lambda (vec i)
		     (vector-set! vec i (fn (vector-ref vec i))))
		   (or not-found
		       (lambda ()
			 (error "key not found")))))

(define (symboltable-update t key fn #!optional not-found)
  (symboltable:_update t key
		   (lambda (vec i)
		     (let ((vec (vector-copy vec)))
		       (vector-set! vec i (fn (vector-ref vec i)))
		       vec))
		   (or not-found
		       (lambda ()
			 (error "key not found")))))

(define (list->symboltable l)
  (let* ((len (length l))
	 (size^ (symboltable:length->size^ len))
	 (vlen (symboltable:size^->vector-length size^))
	 (vec (make-vector vlen #f))
	 (mask (symboltable:size^->mask size^)))
    (let lp ((l l))
      (if (not (null? l))
	  (let* ((a (car l))
		 ;;no fxbitwise-and ?...
		 (slot (bitwise-and (symboltable:key-id (car a)) mask)))
	    (let lp ((i (fxarithmetic-shift slot 1)))
	      (if (vector-ref vec i)
		  (lp (inc2/top i vlen))
		  (begin
		    (vector-set! vec i (car a))
		    (vector-set! vec (inc i) (cdr a)))))
	    (lp (cdr l)))))
    vec))

(TEST
 > (define l '(a b ha))
 > (define t
     (list->symboltable
      (map (lambda (pi)
	     (cons pi
		   (string-append "moo-"
				  (symboltable:key-name pi))))
	   l)))
 > t
 #(#f #f ha "moo-ha" b "moo-b" a "moo-a")
 > (symboltable-ref t 'ha 'not-found)
 "moo-ha"
 > (symboltable-ref t 'hu 'not-found)
 not-found
 > (symboltable-update! t 'ha (lambda (x) 1))
 > (symboltable-ref t 'ha 'not-found)
 1
 > (symboltable-update! t 'ha inc)
 > (symboltable-ref t 'ha 'not-found)
 2
 > (define t2 (symboltable-update t 'ha inc))
 > (symboltable-ref t 'ha 'not-found)
 2
 > (symboltable-ref t2 'ha 'not-found)
 3
 > (symboltable-update t 'hu inc (lambda () 'no))
 no

 )
