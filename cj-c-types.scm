;;; Copyright 2006-2007 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; imports for dev/testing purposes only--usually the cj-c-types.scm file is meant to be include'd.
(require
 ;; the user including cj-c-types.scm must add these manually to his own module requirements!
 cj-env
 cj-gambit-sys
 )
;; (compile #t)


;; cj Sun, 05 Nov 2006 16:15:13 +0100
;; Some types I'm going to use frequently, for inclu'sion into user modules.

;; BTW there are several ways to represent addresses:
;; - wordaddress: for word-aligned addresses. represented by fixnums. be careful with calculations.
;; - void*: foreign object. typesafe. (and directly (+-automatically) supported by Gambit)
;;      - BUT: only works for still(/perm) scheme objects and C objects.
;;        (otherwise between time of creation and time of usage it may already be invalid)
;;        That's bad because I may want to feed (also short) scheme strings
;;        to write() directly etc.
;;        However even with non-allocating ways to extract the address movable
;;        objects are problematic, at least not interrupts-enabled has to be declared.
;; - number: requires bignum allocations. still type unsafe (and movable objects impossible)
;; - an u32vector with 1 cell (I'll call it the (boxed) u32 type):
;;      + regular, as wordaddress and void* are.
;;      + ___BODY() can be given as address to C functions as "output argument".
;;      + similarly used already for pipe() in cj-posix.
;;      + maybe will write library to deal with anyway for numerics project.
;;      + can be preallocated and reused (unlike bignums),
;;           fastest possible for non-aligned addresses
;;    (but also type unsafe)

;; One way for feeding scheme-objects to C functions (as buffers) is: to implement
;; a type dispatch in C, or always feed ___SCMOBJ to C, and make it know how to work with it.

;; AH and I 'forgot' once again that addresses are not always
;; 32bit. Has to have another name.

;; ==== fixnum, wordaddress ============================================

(c-declare "//ewig massig 8 makros  4 paare . Ein paar von paaren für jede callrichtung.
#define ___BEGIN_CFUN_FIXNUM_to_WORD(obj, val, pos) \
   if ((___err= ___FIX(___FIXNUMP(obj) ? ___NO_ERR : ___STOC_INT_ERR)) == ___FIX(___NO_ERR)) { \
       val=___INT(obj); /* ç ___STOC_INT_ERR ist wohl int bezogn..*/

#define ___END_CFUN_FIXNUM_to_WORD(obj, val, pos) \
   }


#define bitsizeof(x) (CHAR_BIT * sizeof(x))

#define ___BEGIN_CFUN_WORD_to_FIXNUM(val, obj) \
   { ___WORD tmp1= (val); ___WORD tmp2= (tmp1 & (7<<(bitsizeof(___WORD)-3))); \
     if ((tmp2==0) || (tmp2==(7<<(bitsizeof(___WORD)-3)))) { \
        ___err= ___FIX(___NO_ERR); obj= ___FIX(tmp1);

#define ___END_CFUN_WORD_to_FIXNUM(val, obj) \
     } else { ___err= ___FIX(___CTOS_INT_ERR); /* Can't convert from C int */ } \
   }

/*
#define ___BEGIN_SFUN_WORD_to_FIXNUM(val, obj, pos)
#define ___END_SFUN_WORD_to_FIXNUM(val, obj, pos)

#define ___BEGIN_SFUN_FIXNUM_to_WORD(obj, val)
#define ___END_SFUN_FIXNUM_to_WORD(obj, val)
*/
")
; (define bitsize-of-word ((c-lambda ()
; 				   int
; 				   "___result= bitsizeof(___WORD);")))
;; 32

(c-define-type fixnum "___WORD" "WORD_to_FIXNUM" "FIXNUM_to_WORD" #f) ;; no cleanup. right?.

;; OCH ich tubel realisiere dass ich ja einen wordaddress typ, nicht einen fixnum typ will.
;;grrrrrr
;;ah dann merk ich dass ich fixnum DOCH AUCH brauche staun..

(c-declare "//ewig massig 8 makros  4 paare . Ein paar von paaren für jede callrichtung.
#define ___BEGIN_CFUN_FIXNUM_to_ADDRESS(obj, val, pos) if ((___err= (___FIXNUMP(obj) ? ___FIX(___NO_ERR) : ___STOC_TYPE_ERR+pos)) == ___FIX(___NO_ERR)) { val=(void*)obj; /* ç typfehler so gut? */
#define ___END_CFUN_FIXNUM_to_ADDRESS(obj, val, pos) }

#define ___BEGIN_CFUN_ADDRESS_to_FIXNUM(val, obj)  if ((___err= ((((___WORD)val ) & 3)==0) ? ___FIX(___NO_ERR) : ___CTOS_TYPE_ERR) == ___FIX(___NO_ERR)) { obj= (___WORD)val; /* ç? */
#define ___END_CFUN_ADDRESS_to_FIXNUM(val, obj) }

/*
#define ___BEGIN_SFUN_ADDRESS_to_FIXNUM(val, obj, pos)
#define ___END_SFUN_ADDRESS_to_FIXNUM(val, obj, pos)

#define ___BEGIN_SFUN_FIXNUM_to_ADDRESS(obj, val)
#define ___END_SFUN_FIXNUM_to_ADDRESS(obj, val)
*/
")
(c-define-type wordaddress "void*" "ADDRESS_to_FIXNUM" "FIXNUM_to_ADDRESS" #f) ;; no cleanup. right?.

;; ==== void* ========================================================

;; this is for cases where wordaddress won't work because the address is not word aligned.
;; allocates a normal foreign object.

(c-define-type void* (pointer "void" void* #f))
;; (todo: I still don't know(/understand) the cleanup argument to c-define-type. switch off?ornot?)

(c-define-type const_void* void*);; why does this work w/o gcc warning? ?  (todo)

;; ==== U32/U64 address boxes =========================================

;; what's the bit width of a void* ? Use cj-gambit-sys. -> user of
;; cj-c-types must add it to his dependencies, sigh.
;; (Ok since I want those to be inlined it's at least ok to continue the include approach.)

;(define word-width (##c-code "___RESULT= ___FIX(___WORD_WIDTH);"))
; -> 32 or 64 orso.

;; D'oh: also cj-env dependency:
(insert-result-of
 (let ((uXvector (string-append "u" (number->string word-width) "vector")))
   `(begin
      (define addressbox (lambda (val)
			   (,(symbol-append uXvector) val)))
      (define @make-addressbox (lambda ()
				 (,(symbol-append "##make-" uXvector) 1)))
      (define addressbox-ref (lambda (vec)
			       (,(symbol-append uXvector "-ref") vec 0))) ;; prepend "##" ?...no, leave that to gambit's unsafe handling.
      (define addressbox-set! (lambda (vec val)
				(,(symbol-append uXvector "-set!") vec 0 val))))))


;; What's the reason to call those "addressbox" and not "address"
;; alone (which would be a subrange (subtype?) of integer)?  Those are
;; mutable objects. So "box" seems reasonable to me.

;; (At one time I called it something 'inbetween': |addressbox| was
;; called |address&|, |addressbox-ref| was called |*address|. That may
;; be more reasonable than just calling them integer->address or
;; address->integer. (But totally confusing since it's not *really* C
;; code alike?))

;; The nice thing about addressbox is: could also invent an "addressvector".
;; It's simple and consistent.
