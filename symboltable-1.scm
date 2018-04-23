;;; Copyright 2011, 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require (cj-struct-tag struct-tag-allocate! struct-metadata)
	 (cj-env define-if-not-defined)
	 define-macro-star
	 cj-inline
	 cj-source-quasiquote
	 test)

(export (inline symboltable?)
	symboltable-of
	empty-symboltable ;; treat as read-only, please!
	symboltable-length
	symboltable-ref ;; with required alternative value if missing
	@symboltable-ref
	symboltable-xref ;; exception
	symboltable-maybe-ref
	symboltable-contains?
	symboltable-update!
	symboltable-set! ;; only works for keys already in the table
	symboltable-update
	list->symboltable
	list->symboltable-function
	symboltable
	symboltable:fold ;; hm name?
	symboltable:every?
	symboltable->list
	(method symboltable.show)
	symboltable-keys
	symboltable-update-all
	symboltable-add
	symboltable-set
	symboltable-remove
	symboltable-copy ;; clone

	#!optional
	(macro (symboltable-declare))
	;; fake inline; requires symboltable-declare !
	(macro @symboltable-ref-inline)
	)

(include "cj-standarddeclares.scm")

;;;
;;;; Hash tables with symbols as keys
;;;

;; Those are implemented as flat vectors. Not using a wrapper
;; prohibits mutators that change the number of elements in the
;; hash. But OTOH, restricting it this way also means that it's
;; automatically thread-safe (well, it can *lose* changes in race
;; conditions, but it will never leave a corrupt table, and often
;; readers (and in some cases writers) never need to aquire a lock).



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



;; automatically chose implementation in C when compiled
(compile-time
 (define use-implementation-in-c? (mod:compiled?)))


(IF use-implementation-in-c?
    (begin
      ;; keep in sync with definition in C, ___keyp
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

(define-if-not-defined symboltable:tag
  (struct-tag-allocate! 'symboltable
			(struct-metadata 'symboltable)))

(define-inline (symboltable? x)
  ;; XX Gambit: does length never overflow the fixnum range?
  (declare (standard-bindings)
  	   (extended-bindings)
  	   (not safe))
  (and (##vector? x)
       (fx>= (vector-length x) 4)
       ;; ^ not 2: need an empty slot or (vector symboltable:tag 1) or
       ;; so would lead to endless loop in C. Oh well, and make safe
       ;; by type checking slot 1 too?
       (eq? (vector-ref x 0) symboltable:tag)
       (fixnum? (vector-ref x 1))))

(TEST
 > (vector? (list->symboltable '()))
 #f
 > (symboltable? (list->symboltable '()))
 #t)



(define-if-not-defined empty-symboltable
  (vector symboltable:tag 0 #f #f))


;; XX move to common place?
(define (type-error v)
  (error "invalid type: " v))


;; length, size, vector-length, and masks
;;
;; length= number of keys contained in table
;; size= number of slots for keys in the table (always > length)
;; vector-length= 2*size+1

;; at which vector position the data starts
;; XX keep in sync with definition in C, TABLE_BASE_I !
(define symboltable:base-i 2)

(define (symboltable:inc2/top n top)
  (inc2/top+bottom n top symboltable:base-i))
(define (symboltable:inc-to-base n)
  (fx+ n symboltable:base-i))
(define (symboltable:dec-from-base n)
  (fx- n symboltable:base-i))

(define (@symboltable-length t)
  (vector-ref t 1))
(define (@symboltable-length-set! t v)
  (vector-set! t 1 v))


(define (symboltable-length t)
  (if (symboltable? t)
      (@symboltable-length t)
      (type-error t)))

(TEST
 ;; fxlength from Gambit (vs. bit-count)
 > (map fxlength '(256 255 4 3 2 1 0 -1 -2 -3))
 (9 8 3 2 2 1 0 0 1 2))

(define (symboltable:length->size^ len)
  (fxlength (+ len (fxarithmetic-shift len -2))))

(define (symboltable:size^->vector-length size)
  (symboltable:inc-to-base (fxarithmetic-shift 1 (inc size))))
;; ^-inverse-v (for allowed values)
(define (symboltable:vector-length->size^ vlen)
  (symboltable:dec-from-base (fxlength (dec vlen))))

(define (symboltable:length->vector-length v)
  (symboltable:size^->vector-length
   (symboltable:length->size^ v)))

(TEST
 > (symboltable:length->vector-length 0)
 4
 > (symboltable:length->vector-length 1)
 6
 > (symboltable:length->vector-length 2)
 10
 > (symboltable:length->vector-length 3)
 10
 > (symboltable:length->vector-length 4)
 18
 > (symboltable:length->vector-length 6)
 18
 > (symboltable:length->vector-length 7)
 34
 > (symboltable:length->vector-length 12)
 34
 > (symboltable:length->vector-length 13)
 66
 )

(define (@make-symboltable/len+vlen len vlen)
  (let ((v (make-vector vlen #f)))
    (vector-set! v 0 symboltable:tag)
    (@symboltable-length-set! v len)
    v))

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
      (let lp ((i (symboltable:inc-to-base (fxarithmetic-shift slot 1))))
	;;(warn "   lp: i,key2=" i (vector-ref vec i))
	(cond ((vector-ref vec i)
	       => (lambda (key*)
		    (if (symboltable:key-eq? key* key)
			(handle-found vec (inc i))
			(lp (symboltable:inc2/top i vlen)))))
	      (else
	       (handle-not-found)))))))

(define (symboltable-ref:scheme t key alternate-value)
  (symboltable:_update t key vector-ref (lambda ()
					  alternate-value)))


(define symboltable-ref:c:invalid-type (gensym))

(define-macro* (symboltable-declare)
  (if (mod:compiled?)
      `(c-declare "
___SCMOBJ symboltable_1__symboltable_ref (___SCMOBJ t, ___SCMOBJ key, ___SCMOBJ alternate_value);
")
      `(begin)))

(define-macro* (symboltable-init-scope)
  `(begin
     (symboltable-declare)
     (c-declare "
#define likely(expr) __builtin_expect(expr, 1)
#define unlikely(expr) __builtin_expect(expr, 0)
")

     (c-declare "
#define ___keyp ___SYMBOLP
")

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
")
     (c-declare "___SCMOBJ ___error_invalid_type= 0;")
     (##c-code "___error_invalid_type=___ARG1;" symboltable-ref:c:invalid-type)

     (c-declare "
static inline
int ___inc2_with_top_bottom(int n, int top, int bottom) {
    n= n + 2;
    return (n >= top) ? bottom : n;
}

#define TABLE_BASE_I 2

static inline
int ___table_inc2_with_top(int n, int top) {
    return ___inc2_with_top_bottom(n, top, TABLE_BASE_I);
}

")))

(IF use-implementation-in-c?
    
    (begin
      (symboltable-init-scope)

      ;;  (define-typed (symboltable-ref:c #(symboltable? t) key alternate-value)

      (c-declare "
#define ___key_id(v) ___INT(___VECTORREF(v,___FIX(1)))
// lol same

___SCMOBJ symboltable_1__symboltable_ref (___SCMOBJ t, ___SCMOBJ key, ___SCMOBJ alternate_value) {
    ___WORD ___temp; /* used by ___keyp */

    /* can't check table type anymore easily since its head is a movable object now
    if (unlikely(! ___tablep(t))) {
	return ___error_invalid_type;
    }
    */

    if (unlikely(! ___keyp(key))) {
	return ___error_invalid_type;
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

    ___WORD i= (slot << 1) + TABLE_BASE_I;
    lp: {
	___SCMOBJ key2 = ___VECTORREF(vec,___FIX(i));
	// printf(\"   lp:  i=%ld, key2=%ld\\n\", i,key2);
	if (unlikely (key2 == ___FAL)) {
	    return alternate_value;
	} else {
    /*
	    if (unlikely(! ___keyp(key2))) {
		return ___error_invalid_type;
	    }
    */
	    if (key2 == key) { // XX only works for interned symbols!
		return ___VECTORREF(vec,___FIX(i+1));
	    } else {
		i= ___table_inc2_with_top(i, vlen);
		goto lp;
	    }
	}
    }
}
")
      
      (define-macro* (@symboltable-ref:c-lambda)
	(quasiquote-source
	 (lambda (t key alternate-value)
	   (let ((res (##c-code "___RESULT= symboltable_1__symboltable_ref (___ARG1, ___ARG2, ___ARG3);"
				t key alternate-value)))
	     (if (eq? res symboltable-ref:c:invalid-type)
		 (error "invalid type")
		 res)))))

      (define @symboltable-ref:c (@symboltable-ref:c-lambda))

      (define (symboltable-ref:c t key alternate-value)
	(declare (standard-bindings)
		 (extended-bindings)
		 (not safe))
	(if (symboltable?-inline t)
	    (let ()
	      (declare (not inline) ;; <-no effect, huh; this one does:
		       (inlining-limit 0))
	      (@symboltable-ref:c t key alternate-value))
	    (error "not a symboltable:" t)))

      (define symboltable-ref symboltable-ref:c) ;; choose
      (define @symboltable-ref @symboltable-ref:c)
      (define-macro* (@symboltable-ref-inline t key alternate-value)
	(if (mod:compiled?)
	    `((@symboltable-ref:c-lambda) ,t ,key ,alternate-value)
	    `(@symboltable-ref ,t ,key ,alternate-value))))
    

    (begin						;; not compile
      (define symboltable-ref:c symboltable-ref:scheme) ;; fake
      (define symboltable-ref symboltable-ref:scheme)
      (define @symboltable-ref symboltable-ref)))

(define symboltable:nothing (gensym 'nothing))

(define (symboltable-xref t key)
  (let ((res (symboltable-ref t key symboltable:nothing)))
    (if (eq? res symboltable:nothing)
	(error "key not found:" key)
	res)))

(define (symboltable-maybe-ref t key)
  (symboltable-ref t key #f))

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

;; only works for keys that already are in the table (for now)
(define (symboltable-set! t key val)
  (symboltable:_update t key
		       (lambda (vec i)
			 (vector-set! vec i val))
		       symboltable:error-key-not-found))

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
	 (vec (@make-symboltable/len+vlen len vlen))
	 (mask (symboltable:size^->mask size^)))
    (let lp ((l l))
      (if (not (null? l))
	  (let* ((a (car l))
		 ;;no fxbitwise-and ?...
		 (slot (bitwise-and (symboltable:key-id (car a)) mask)))
	    (let lp ((i (symboltable:inc-to-base (fxarithmetic-shift slot 1))))
	      (if (vector-ref vec i)
		  (lp (symboltable:inc2/top i vlen))
		  (begin
		    (vector-set! vec i (car a))
		    (vector-set! vec (inc i) (cdr a)))))
	    (lp (cdr l)))))
    vec))

(define (list->symboltable-function l #!optional alternate-value)
  (let ((t (list->symboltable l)))
    (lambda (k)
      (symboltable-ref t k alternate-value))))

(define (symboltable . l)
  (list->symboltable l))

(define (symboltable:fold t tail fn)
  (let ((vlen (symboltable:vector-length t))
	(vec t))
    (let lp ((i (symboltable:dec-from-base vlen))
	     (res tail))
      (let lp2 ((i i))
	(if (>= i symboltable:base-i)
	    (cond ((vector-ref vec i)
		   => (lambda (key)
			(lp (dec2 i)
			    (fn key
				(vector-ref vec (inc i))
				res))))
		  (else
		   (lp2 (dec2 i))))
	    res)))))

(define (symboltable:every? t pred/2)
  (let ((vlen (symboltable:vector-length t))
	(vec t))
    (let lp ((i symboltable:base-i))
      (if (< i vlen)
	  (cond ((vector-ref vec i)
		 => (lambda (key)
		      (and (pred/2 key (vector-ref vec (inc i)))
			   (lp (inc2 i)))))
		(else
		 (lp (inc2 i))))
	  #t))))

(define (symboltable->list t #!optional (tail '()))
  (symboltable:fold t tail (lambda (k v r)
			     (cons (cons k v)
				   r))))


(define (symboltable-keys t #!optional (tail '()))
  (symboltable:fold t tail (lambda (k v r)
			     (cons k
				   r))))

;; quasi combination of fold and update:
(define (symboltable-update-all t fn/2)
  ;; (largely copy from symboltable:fold, sigh)
  (let ((vlen (symboltable:vector-length t))
	(vec t))
    (let ((newvec (@make-symboltable/len+vlen (@symboltable-length vec) vlen)))
      (let lp ((i (symboltable:dec-from-base vlen)))
	(let lp2 ((i i))
	  (if (fx>= i symboltable:base-i)
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
	      (begin
		(if (= old-vlen vlen)
		    (let ((v (vector-copy old-vec)))
		      (@symboltable-length-set! v newlen)
		      v)
		    ;; fold and add!
		    (let ((new-vec (@make-symboltable/len+vlen newlen vlen)))
		      (let lp ((old-i symboltable:base-i))
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
					      ((i (symboltable:inc-to-base
						   (fxarithmetic-shift
						    slot 1))))
					    (if (vector-ref new-vec i)
						;; no eq? check necessary
						(lp (symboltable:inc2/top i vlen))
						(begin
						  (vector-set! new-vec i k)
						  (vector-set!
						   new-vec
						   (inc i) val))))))))))
			
			      (lp (inc2 old-i)))
			    new-vec)))))))
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
	 (let lp ((i (symboltable:inc-to-base (fxarithmetic-shift slot 1))))
	   ;;(warn "   lp: i,key2=" i (vector-ref vec i))
	   (cond ((vector-ref vec i)
		  => (lambda (key*)
		       (if (symboltable:key-eq? key* key)
			   (handle-found vec i key)
			   (lp (symboltable:inc2/top i vlen)))))
		 (else
		  (handle-not-found vec i key)))))))))


(define (symboltable-set t key val)
  (let ((v (symboltable-ref t key symboltable:nothing)))
    (if (eq? v symboltable:nothing)
	(symboltable-add t key val)
	(if (eq? v val)
	    t
	    (let ((t* (vector-copy t)))
	      (symboltable-update! t* key
				   (lambda (_)
				     val))
	      t*)))))


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
		       (let takeout! ((i (symboltable:inc2/top i vlen))
				      (items '()))
			 ;; (warn "   takeout!: i,key2=" i (vector-ref vec i))
			 (cond
			  ((vector-ref vec i)
			   => (lambda (key*)
				(let ((val (vector-ref vec (inc i))))
				  ;; delete
				  (vector-set! vec i #f)
				  (vector-set! vec (inc i) #f)
				  (takeout! (symboltable:inc2/top i vlen)
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
					       (symboltable:inc-to-base
						(fxarithmetic-shift slot
								    1)))))
				       ;; (warn "   reinsert!: i,key,keyslot="
				       ;; 	   i key (vector-ref vec i))
				       (cond
					((vector-ref vec i)
					 => (lambda (key*)
					      (if (symboltable:key-eq? key* key)
						  (error "BUG")
						  (lp (symboltable:inc2/top
						       i vlen)))))
					(else
					 ;; (XX the usual 2 writing statmts..)
					 (vector-set! vec i key)
					 (vector-set! vec (inc i) val)
					 (reinsert! items*))))))
				   ;; done
				   (void)))))))
		       vec)))
		 ;; XX still copy paste, basically:
		 (let lp ((i (symboltable:inc-to-base
			      (fxarithmetic-shift slot 1))))
		   ;;(warn "   lp: i,key2=" i (vector-ref vec i))
		   (cond ((vector-ref vec i)
			  => (lambda (key*)
			       (if (symboltable:key-eq? key* key)
				   (handle-found vec i key)
				   (lp (symboltable:inc2/top i vlen)))))
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


(define (symboltable-of pred)
  (let ((pred* (lambda (key val)
		 (pred val))))
    (lambda (v)
      (and (symboltable? v)
	   (symboltable:every? v pred*)))))


(define (symboltable-copy v)
  (if (symboltable? v)
      (vector-copy v)
      (error "not a symboltable:" v)))


;; test always run to make sure changes in Gambit won't make
;; serialized symboltables fail undetected:

(assert (= (symboltable:key-id 'a)
	   509649879))
(assert (= (symboltable:key-id 'gehvoie0g280dfasdfasfabjl)
	   465590930))

