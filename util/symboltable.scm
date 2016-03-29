;;; Copyright 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require easy
	 test)

;;;
;;;; Hash tables with symbols as keys
;;;

(compile-time
 (define use-implementation-in-c? (mod:compiled?)))

(IF use-implementation-in-c?
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
(IF use-implementation-in-c?
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

(IF use-implementation-in-c?
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

(define symboltable:nothing (gensym 'nothing))

(define (symboltable-refx t key)
  (let ((res (symboltable-ref t key symboltable:nothing)))
    (if (eq? res symboltable:nothing)
	(error "key not found:" key)
	res)))

(define (symboltable-contains? t key)
  (not (eq? (symboltable-ref t key symboltable:nothing)
	    symboltable:nothing)))

(define symboltable:error-key-not-found
  (lambda ()
    (error "key not found")))

(define (symboltable-update! t key fn #!optional not-found)
  (symboltable:_update t key
		       (lambda (vec i)
			 (vector-set! vec i (fn (vector-ref vec i))))
		       (or not-found symboltable:error-key-not-found)))

(define (symboltable-update t key fn #!optional not-found)
  (symboltable:_update t key
		       (lambda (vec i)
			 (let ((vec (vector-copy vec)))
			   (vector-set! vec i (fn (vector-ref vec i)))
			   vec))
		       (or not-found symboltable:error-key-not-found)))

;; allow fn to pass on an additional value
(define (symboltable-update* t key fn #!optional not-found)
  (symboltable:_update t key
		       (lambda (vec i)
			 (let ((vec (vector-copy vec)))
			   (letv ((val ret2) (fn (vector-ref vec i)))
				 (vector-set! vec i val)
				 (values vec ret2))))
		       (or not-found symboltable:error-key-not-found)))

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

(define (symboltable-keys t #!optional (tail '()))
  (symboltable:fold t tail (lambda (k v r)
			     (cons k
				   r))))

(define (symboltable-sortedkeys t #!optional (tail '()))
  (cmp-sort (symboltable-keys t tail) symbol-cmp))


;; quasi combination of fold and update:
(define (symboltable-update-all t fn/2)
  ;; (largely copy from symboltable:fold, sigh)
  (let ((vlen (symboltable:vector-length t))
	(vec t))
    (let ((newvec (make-vector vlen #f)))
      (vector-set! newvec 0 (vector-ref vec 0))
      (let lp ((i (dec2 vlen)))
	(let lp2 ((i i))
	  (if (positive? i)
	      (cond ((vector-ref vec i)
		     => (lambda (key)
			  ;; (the core of the difference)
			  (vector-set! newvec i key)
			  (vector-set! newvec (inc i)
				       (fn/2 key
					     (vector-ref vec (inc i))))
			  (lp (dec2 i))))
		    (else
		     (lp2 (dec2 i))))
	      newvec))))))

(define _symboltable-add/del-prepare
  (lambda (doing-remove? t key cont)
    ;; careful: to be removed key *has* to be in table
    (let* ((old-vlen (symboltable:vector-length t))
	   (old-vec t)
	   (id (symboltable:key-id key))
	   ;; new length
	   (newlen ((if doing-remove?
			dec
			inc)
		    (symboltable-length t))))
      (let* ((size^ (symboltable:length->size^ newlen))
	     (vlen (symboltable:size^->vector-length size^))
	     ;; slot for new vector
	   
	     (mask (symboltable:size^->mask size^))
	     (slot (bitwise-and id mask))
	     (vec
	      (if (= old-vlen vlen)
		  (vector-copy old-vec)
		  ;; fold and add!
		  (let ((new-vec (make-vector vlen #f)))
		    (let lp ((old-i 1))
		      (if (< old-i old-vlen)
			  (begin
			    (cond
			     ((vector-ref old-vec old-i)
			      =>
			      (lambda (k)
				(if (and doing-remove?
					 (symboltable:key-eq? k key))
				    (void)
				    (let ((val (vector-ref old-vec
							   (inc old-i))))
				      (let* ((id (symboltable:key-id k))
					     (slot (bitwise-and id mask)))
					(let lp
					    ((i (inc
						 (fxarithmetic-shift
						  slot 1))))
					  (if (vector-ref new-vec i)
					      ;; no eq? check necessary
					      (lp (inc2/top+bottom i vlen 1))
					      (begin
						(vector-set! new-vec i k)
						(vector-set!
						 new-vec
						 (inc i) val))))))))))
			
			    (lp (inc2 old-i)))
			  new-vec))))))
	(vector-set! vec 0 newlen)
	(cont old-vlen
	      vlen
	      slot
	      vec)))))


(define symboltable-add
  ;; key must not be contained in the table already
  (lambda (t key val)
    (_symboltable-add/del-prepare
     #f
     t key
     (lambda (old-vlen
	      vlen
	      slot
	      vec)
       (let ((handle-found
	      (lambda (vec i key)
		(error "key already in table:" key)))
	     (handle-not-found
	      (lambda (vec i key)
		(vector-set! vec i key)
		(vector-set! vec (inc i) val)
		vec)))
	 ;; almost copypaste from _update...
	 ;;(warn "vlen,slot=" vlen slot)
	 (let lp ((i (inc (fxarithmetic-shift slot 1))))
	   ;;(warn "   lp: i,key2=" i (vector-ref vec i))
	   (cond ((vector-ref vec i)
		  => (lambda (key*)
		       (if (symboltable:key-eq? key* key)
			   (handle-found vec i key)
			   (lp (inc2/top+bottom i vlen 1)))))
		 (else
		  (handle-not-found vec i key)))))))))


;; (Note: there's no hysteresis implemented for the resizing, up and
;; down is at same number of keys.)

(define symboltable-delete
  ;; key must be contained in the table
  (lambda (t key)
    (if (symboltable-contains? t key)
	(_symboltable-add/del-prepare
	 #t
	 t key
	 (lambda (old-vlen
		  vlen
		  slot
		  vec)
	   (if (not (= old-vlen vlen))
	       ;; already done
	       vec
	       ;; otherwise, remove slot, and reorder elements thereafter
	       (letrec
		   ((handle-found
		     (lambda (vec i key)
		       ;; (warn "   removing: i,key="i key)
		       (vector-set! vec i #f)
		       (vector-set! vec (inc i) #f)
		       ;; also remove all subsequent items:
		       ;; XX another of those almost copypastes:
		       (let takeout! ((i (inc2/top+bottom i vlen 1))
				      (items '()))
			 ;; (warn "   takeout!: i,key2=" i (vector-ref vec i))
			 (cond
			  ((vector-ref vec i)
			   => (lambda (key*)
				(let ((val (vector-ref vec (inc i))))
				  ;; delete
				  (vector-set! vec i #f)
				  (vector-set! vec (inc i) #f)
				  (takeout! (inc2/top+bottom i vlen 1)
					    (cons (cons key*
							val)
						  items)))))
			  (else
			   ;; re-insert them
			   ;; (warn "   start reinsert items: " items)
			   ;; XX yet another of those almosties:
			   ;; don't want a vec copy here, also no
			   ;; changes to symboltable-length, also
			   ;; no need to refetch vlen.
			   (let* ((size^ (symboltable:vector-length->size^
					  vlen))
				  (mask (symboltable:size^->mask size^)))
			     (let reinsert! ((items items))
			       (if (pair? items)
				   (let-pair
				    ((k+v items*) items)
				    (let-pair
				     ((key val) k+v)
				     (let lp
					 ((i (let* ((id (symboltable:key-id key))
						    (slot (bitwise-and id mask)))
					       (inc (fxarithmetic-shift slot
									1)))))
				       ;; (warn "   reinsert!: i,key,keyslot="
				       ;; 	   i key (vector-ref vec i))
				       (cond
					((vector-ref vec i)
					 => (lambda (key*)
					      (if (symboltable:key-eq? key* key)
						  (error "BUG")
						  (lp (inc2/top+bottom
						       i vlen 1)))))
					(else
					 ;; (XX the usual 2 writing statmts..)
					 (vector-set! vec i key)
					 (vector-set! vec (inc i) val)
					 (reinsert! items*))))))
				   ;; done
				   (void)))))))
		       vec)))
		 ;; XX still copy paste, basically:
		 (let lp ((i (inc (fxarithmetic-shift slot 1))))
		   ;;(warn "   lp: i,key2=" i (vector-ref vec i))
		   (cond ((vector-ref vec i)
			  => (lambda (key*)
			       (if (symboltable:key-eq? key* key)
				   (handle-found vec i key)
				   (lp (inc2/top+bottom i vlen 1)))))
			 (else
			  ;; XX cannot ever happen now that we check
			  ;; with contains? first, right?
			  (error "bug" key))))))))
	t)))

(define (symboltable-remove t key)
  (let ((t* (symboltable-delete t key)))
    (if (eq? t* t)
	(error "key not in table:" key)
	t*)))

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
 > (define-values (t3 res)
     (symboltable-update* t2 'ha (lambda (v)
				   (values (inc v)
					   (/ v)))))
 > (symboltable-ref t3 'ha 'not-found)
 4
 > res
 1/3

 > t
 #(3 #f #f ha 2 b "moo-b" a "moo-a")
 > (define t2 (symboltable-add t 'c "moo-c"))
 > t2
 #(4 #f #f ha 2 #f #f #f #f #f #f c "moo-c" b "moo-b" a "moo-a")
 > (symboltable-remove t2 'c)
 #(3 #f #f ha 2 b "moo-b" a "moo-a")
 > (symboltable-remove t2 'a)
 #(3 #f #f ha 2 c "moo-c" b "moo-b")
 > (symboltable-remove t2 'b)
 #(3 #f #f ha 2 c "moo-c" a "moo-a")
 > (symboltable-remove (symboltable-remove t2 'a) 'b)
 #(2 #f #f ha 2 c "moo-c" #f #f)
 > (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'c)
 #(1 #f #f ha 2)
 > (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'ha)
 #(1 #f #f c "moo-c")
 > (symboltable-remove (symboltable-remove (symboltable-remove (symboltable-remove t2 'a) 'b) 'ha) 'c)
 #(0 #f #f)
 > (%try-error (symboltable-remove empty-symboltable 'c))
 #(error "key not in table:" c)

 > (symboltable-add t2 'd "moo-d")
 #(5 #f #f ha 2 #f #f #f #f d "moo-d" c "moo-c" b "moo-b" a "moo-a")

 > (%try-error (symboltable-add t 'b "moo-c"))
 #(error "key already in table:" b)
 > (%try-error (symboltable-remove t 'nono))
 #(error "key not in table:" nono)

 ;; test removal of non-existing keys in the case where the table
 ;; would be resized:
 > (list->symboltable '((a . 1) (b . 2)))
 #(2 #f #f #f #f b 2 a 1)
 > (symboltable-remove # 'a)
 #(1 b 2 #f #f)
 > (%try-error (symboltable-remove # 'a))
 #(error "key not in table:" a)
 ;; and not #(0 b 2)
 > (list->symboltable '((a . 1) (b . 2)))
 #(2 #f #f #f #f b 2 a 1)
 > (symboltable-remove # 'b)
 #(1 #f #f a 1)
 > (%try-error (symboltable-remove # 'b))
 #(error "key not in table:" b)
 ;; and not #(0 a 1)
 )

;; test always run to make sure changes in Gambit won't make
;; serialized symboltables fail undetected:

(assert (= (symboltable:key-id 'a)
	   509649879))
(assert (= (symboltable:key-id 'gehvoie0g280dfasdfasfabjl)
	   465590930))

