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

(compile-time
 (define compile? #f))

(IF compile?
    (begin
      
      (c-declare "
#define likely(expr) __builtin_expect(expr, 1)
#define unlikely(expr) __builtin_expect(expr, 0)
")

      (c-declare "
#define ___keyp ___SYMBOLP
")
      (define (symboltable:key?:c x)
	(##c-code "___RESULT= ___keyp(___ARG1) ? ___TRU : ___FAL;" x))))

(define symboltable:key? symbol?)

(define symboltable:key-eq?
  eq?)

(define (symboltable:key-id v)
  (if (symboltable:key? v)
      (##symbol-hash v)
      (error "invalid type")))

(define symboltable:key-name symbol->string)

(define (symboltable? x)
  (and (vector? x)
       (let ((len (vector-length x)))
	 (and (>= len 3)
	      (odd? len)
	      (fixnum? (vector-ref x 0))))))
;; XX: keep in sync with above definition!
(IF compile?
    (c-declare "
static inline
int ___tablep(___SCMOBJ x)
{
    ___WORD ___temp;
    if (___VECTORP(x)) {
        ___WORD vlen = ___INT(___VECTORLENGTH(x));
        return ((vlen >= 3)
		&& (vlen & 1)
		&& ___FIXNUMP(___VECTORREF(x, ___FIX(0))));
    } else {
        return 0;
    }
}
"))

(define empty-symboltable '#(0 #f #f))


;; XX move to common place?
(define (type-error v)
  (error "invalid type: " v))


;; length, size, vector-length, and masks
;;
;; length= number of keys contained in table
;; size= number of slots for keys in the table (always > length)
;; vector-length= 2*size+1

(define (symboltable-length t)
  (if (symboltable? t)
      (vector-ref t 0)
      (type-error t)))

(define (symboltable:length->size^ len)
  (fxlength (+ len (fxarithmetic-shift len -2))))

(define (symboltable:size^->vector-length size)
  (inc (fxarithmetic-shift 1 (inc size))))
;; ^-inverse-v (for allowed values)
(define (symboltable:vector-length->size^ vlen)
  (fx- (fxlength (dec vlen)) 2))

(define symboltable:length->vector-length
  (compose symboltable:size^->vector-length
	   symboltable:length->size^))

(TEST
 > (symboltable:length->vector-length 0)
 3
 > (symboltable:length->vector-length 1)
 5
 > (symboltable:length->vector-length 2)
 9
 > (symboltable:length->vector-length 3)
 9
 > (symboltable:length->vector-length 4)
 17
 > (symboltable:length->vector-length 6)
 17
 > (symboltable:length->vector-length 7)
 33
 > (symboltable:length->vector-length 12)
 33
 > (symboltable:length->vector-length 13)
 65
 )

;; lib
(define (inc2 n)
  (fx+ n 2))

(define (dec2 n)
  (fx- n 2))

(define (inc2/top+bottom n top bottom)
  (let ((n (inc2 n)))
    (if (fx>= n top)
	bottom
	n)))
;;/ lib

(define (symboltable:vector-length v)
  (if (symboltable? v)
      (vector-length v)
      (type-error v)))

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
      (let lp ((i (inc (fxarithmetic-shift slot 1))))
	;;(warn "   lp: i,key2=" i (vector-ref vec i))
	(cond ((vector-ref vec i)
	       => (lambda (key*)
		    (if (symboltable:key-eq? key* key)
			(handle-found vec (inc i))
			(lp (inc2/top+bottom i vlen 1)))))
	      (else
	       (handle-not-found)))))))

(define (symboltable-ref:scheme t key alternate-value)
  (symboltable:_update t key vector-ref (lambda ()
					  alternate-value)))

(IF compile?
    (begin

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

if (unlikely(! ___key_id(key))) {
    ___RESULT= ___error_invalid_type;
    goto end;
}

___SCMOBJ vec = t;
___WORD vlen = ___INT(___VECTORLENGTH(vec));
/* when compiling with -O3, the length retrieval might be merged
   with the one from ___tablep */

___WORD id = ___key_id(key);
// (define-inline (symboltable:vector-length->size^ vlen)
//    (fx- (fxlength vlen) 2))
___WORD sizepot = ___fxlength(vlen)  - 2;
// (define-inline (symboltable:size^->mask size^)
//   (dec (fxarithmetic-shift 1 size^)))
___WORD mask = (1 << sizepot)-1;
___WORD slot = id & mask;

// printf(\"vlen=%ld, id=%ld, sizepot=%ld, mask=%ld, slot=%ld\\n\", vlen,id,sizepot,mask,slot);

___WORD i= (slot << 1)+1;
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
            if (i >= vlen) i=1;
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
)

(begin ;; not compile
  (define symboltable-ref:c symboltable-ref:scheme) ;; fake
  (define symboltable-ref symboltable-ref:scheme)))


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
    (vector-set! vec 0 len)
    (let lp ((l l))
      (if (not (null? l))
	  (let* ((a (car l))
		 ;;no fxbitwise-and ?...
		 (slot (bitwise-and (symboltable:key-id (car a)) mask)))
	    (let lp ((i (inc (fxarithmetic-shift slot 1))))
	      (if (vector-ref vec i)
		  (lp (inc2/top+bottom i vlen 1))
		  (begin
		    (vector-set! vec i (car a))
		    (vector-set! vec (inc i) (cdr a)))))
	    (lp (cdr l)))))
    vec))

(define (symboltable:fold t tail fn)
  (let ((vlen (symboltable:vector-length t))
	(vec t))
    (let lp ((i (dec2 vlen))
	     (res tail))
      (let lp2 ((i i))
	(if (positive? i)
	    (cond ((vector-ref vec i)
		   => (lambda (key)
			(lp (dec2 i)
			    (fn key
				(vector-ref vec (inc i))
				res))))
		  (else
		   (lp2 (dec2 i))))
	    res)))))

(define (symboltable->list t #!optional (tail '()))
  (symboltable:fold t tail (lambda (k v r)
			     (cons (cons k v)
				   r))))

(define symboltable-add
  (lambda (t key val)
    ;; key must not be contained in the table already
    (let* ((old-vlen (symboltable:vector-length t))
	   (old-vec t)
	   (id (symboltable:key-id key))
	   ;; new length
	   (newlen (inc (symboltable-length t)))
	   (size^ (symboltable:length->size^ newlen))
	   (vlen (symboltable:size^->vector-length size^))
	   ;; slot for new vector
	   (mask (symboltable:size^->mask size^))
	   (slot (bitwise-and id mask))
	   (vec
	    (if (= old-vlen vlen)
		(vector-copy old-vec)
		;; fold and add!
		;;still checking whether the old key is already present--ehrdum.jeder der walkten keys IST unique.
		(let ((new-vec (make-vector vlen #f)))
		  (let lp ((old-i 1))
		    (if (< old-i old-vlen)
			(begin
			  (cond ((vector-ref old-vec old-i)
				=>
				(lambda (key)
				  (let ((val (vector-ref old-vec
							 (inc old-i))))
				    (let* ((id (symboltable:key-id key))
					   (slot (bitwise-and id mask)))
				      (let lp
					  ((i (inc
					       (fxarithmetic-shift slot 1))))
					(if (vector-ref new-vec i)
					    ;; no eq? check necessary
					    (lp (inc2/top+bottom i vlen 1))
					    (begin
					      (vector-set! new-vec i key)
					      (vector-set! new-vec (inc i) val)))))))))
			
			  (lp (inc2 old-i)))
			new-vec))))))
      (vector-set! vec 0 newlen)
      (let ((handle-found
	     (lambda (vec i)
	       (error "key already in table:" key)))
	    (handle-not-found
	     (lambda (vec i)
	       (vector-set! vec i key)
	       (vector-set! vec (inc i) val)
	       vec)))
       ;; copypaste from _update... - almost. handle-not-found is different
       ;;(warn "vlen,slot=" vlen slot)
       (let lp ((i (inc (fxarithmetic-shift slot 1))))
	 ;;(warn "   lp: i,key2=" i (vector-ref vec i))
	 (cond ((vector-ref vec i)
		=> (lambda (key*)
		     (if (symboltable:key-eq? key* key)
			 (handle-found vec (inc i))
			 (lp (inc2/top+bottom i vlen 1)))))
	       (else
		(handle-not-found vec i))))))))

(TEST
 > (symboltable-ref '#(0 #f #f) 'ha 'not-found)
 not-found
 > (map (lambda (v)
	  (map (lambda (ref)
		 (with-exception-catcher
		  (lambda (x) (cond ((range-exception? x) 'range)
				    ((error-exception? x) 'error)
				    ((type-exception? x) 'type)
				    (else x)))
		  (lambda () (ref v 'ha 'not-found))))
	       (list symboltable-ref:scheme symboltable-ref:c)))
	'(foo #() #(#f) #(#f #f) #(#f #f #f) #(0 #f #f) #(0 #f #f #f)))
 ((error error)
  (error error)
  (error error)
  (error error)
  (error error)
  (not-found not-found)
  (error error))
 > (define l '(a b ha))
 > (define t
     (list->symboltable
      (map (lambda (pi)
	     (cons pi
		   (string-append "moo-"
				  (symboltable:key-name pi))))
	   l)))
 > t
 #(3 #f #f ha "moo-ha" b "moo-b" a "moo-a")
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

 > t
 #(3 #f #f ha 2 b "moo-b" a "moo-a")
 > (define t2 (symboltable-add t 'c "moo-c"))
 > t2
 #(4 #f #f ha 2 #f #f #f #f #f #f c "moo-c" b "moo-b" a "moo-a")
 > (symboltable-add t2 'd "moo-d")
 #(5 #f #f ha 2 #f #f #f #f d "moo-d" c "moo-c" b "moo-b" a "moo-a")
 > (with-exception-catcher error-exception-message
			   (thunk (symboltable-add t 'b "moo-c")))
 "key already in table:"
 )


;; Utility library:

;; Symbol hierarchy

(define parent-test:first-time-count 0)

(define parent:nothing (gensym))

(define (maybe-_-symbol stringpart)
  (lambda (splitchar)
    (let ((cache empty-symboltable))
      (lambda (sym)
	(let ((cache* cache))
	  (let ((v (symboltable-ref cache* sym parent:nothing)))
	    (if (eq? v parent:nothing)
		(begin
		  (inc! parent-test:first-time-count)
		  (let* ((str (symbol->string sym))
			 (len (string-length str))
			 (parent (let lp ((i (dec len)))
				   (if (negative? i)
				       #f ;; nil.  !
				       (if (char=? (string-ref str i) splitchar)
					   (string->symbol
					    (stringpart str i len))
					   (lp (dec i)))))))
		    (set! cache
			  (symboltable-add cache* sym parent))
		    parent))
		v)))))))

(define maybe-parent-symbol
  (maybe-_-symbol (lambda (str mid end)
		    (substring str 0 mid))))

(define maybe-leaf-symbol
  (maybe-_-symbol (lambda (str mid end)
		    (substring str (inc mid) end))))

(define maybe-parent-.-symbol (maybe-parent-symbol #\.))
(define maybe-leaf-.-symbol (maybe-leaf-symbol #\.))

(TEST
 > (define parent-test:first-time-count 0)
 > (define maybe-parent-.-symbol (maybe-parent-symbol #\.))
 > parent-test:first-time-count
 0
 > (maybe-parent-.-symbol 'Foo)
 #f
 > parent-test:first-time-count
 1
 > (maybe-parent-.-symbol 'Foo)
 #f
 > parent-test:first-time-count
 1
 > (maybe-parent-.-symbol 'Foo.bar)
 Foo
 > parent-test:first-time-count
 2
 > (maybe-parent-.-symbol 'Foo.bar)
 Foo
 > parent-test:first-time-count
 2
 > (maybe-parent-.-symbol '.bar)
 ||
 > parent-test:first-time-count
 3
 ;; and maybe-leaf-symbol:
 > (define maybe-leaf-%-symbol (maybe-leaf-symbol #\%))
 > (maybe-leaf-%-symbol 'Foo.bar%baz)
 baz
 > parent-test:first-time-count
 4
 > (maybe-leaf-%-symbol 'Foo.bar%baz)
 baz
 > parent-test:first-time-count
 4
 > (maybe-leaf-%-symbol 'Foo.bar%baz%buzz)
 buzz
 > parent-test:first-time-count
 5
 > (maybe-leaf-%-symbol 'Foo.bar%baz%buzz)
 buzz
 > parent-test:first-time-count
 5
 )
