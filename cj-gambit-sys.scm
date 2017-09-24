(require test
	 cj-env	;;XX update what exactly?
	 cj-typed
	 ;;(predicates-1 exact-natural0?) actually not necessary,
	 ;;   c-lambda will check
	 )

;; (compile #t)

(export wordaddress-peek
	body-address
	body-wordaddress
	;;check-mem-allocated
	max-fixnum
	min-fixnum
	mem-address
	mem-wordaddress
	mem-allocated?
	;;mk-wordaddress->address
	subtype
	word-size
	word-width
	head-tag

	still-object?
	vector-like?
	mem-bytes

	maybe-decompile
	decompile
	maybe-procedure-name

	repl
	repl-within

	continuation-location

	;; utilities:
	vectorlike-bytecopy!
	vectorlike-byteequal?
	vectorlike-byteref
	vectorlike-byteset!
	vectorlike-bytefill!

	u32:wordaddr-peek
	u8:wordaddr-peek
	u8:wordaddr-poke

	u8:peek
	u32:peek
	u64:peek
	
	#!optional
	check-mem-allocated
	check-vector-like
	@vectorlike-bytecopy!
	@vectorlike-byteequal?
	@vectorlike-byteref
	@vectorlike-byteset!
	@vectorlike-bytefill!
	copy-to-body@!
	copy-from-body@!
	@memcpy-to-object
	)


(include "cj-standarddeclares.scm")
;; do NOT declare fixnum and not safe; this would break number
;; calculations overflowing fixnums.

(c-declare "#include <string.h>")



;; (define (mem-allocated? obj)
;;   (##c-code "___RESULT= ___BOOLEAN(___MEM_ALLOCATED(___ARG1));" obj))

(define mem-allocated? ##mem-allocated?)

(define (check-mem-allocated obj thunk)
  (if (mem-allocated? obj)
      (thunk)
      (error "not a memory-allocated object:" obj)))


;; WHERE does the pointer point to? the start of the data, or the head?


;;(doesn't gambit have some internal for this already? no, that was foreign objects.)
(define (mem-wordaddress obj) ;; actually returns the *word* address, not byte address; this way we can always return a fixnum, which may be negative.
  (##c-code
   "___RESULT= ___MEM_ALLOCATED(___ARG1)
             ? (___ARG1 &~ ___CAST(___WORD,3)) /* creates a fixnum */
             : ___FAL;"
   obj))

(define (body-wordaddress obj)
  (##c-code
   "___RESULT= ___MEM_ALLOCATED(___ARG1)
             ? (((___WORD)___BODY(___ARG1)) &~ ___CAST(___WORD,3)) /* well is a fixnum already anyway */
             : ___FAL;"
   obj))

(define max-fixnum (##c-code "___RESULT= ___FIX(___MAX_FIX);"))
(TEST
 > (##fixnum? (+ max-fixnum 0))
 #t
 > (##fixnum? (+ max-fixnum 1))
 #f
 )

(define min-fixnum (##c-code "___RESULT= ___FIX(___MIN_FIX);"))
(TEST
 > (##fixnum? (- min-fixnum 0))
 #t
 > (##fixnum? (- min-fixnum 1))
 #f
 )


(define word-size (##c-code "___RESULT= ___FIX(___WS);"));; bytes
(define word-width (##c-code "___RESULT= ___FIX(___WORD_WIDTH);"));; bits
(TEST
 > (= (* word-size 8) word-width)
 #t
 > (= (- (expt 2 (- word-width 3)) 1) max-fixnum)
 #t
 )

(define (mk-wordaddress->address fn)
  (lambda (obj)
    (declare (mostly-fixnum))
    (cond ((fn obj)
	   => (lambda (a)
		(* word-size
		   (if (>= a 0)
		       a
		       (+ max-fixnum a)))))
	  (else #f))))

(define mem-address (mk-wordaddress->address mem-wordaddress))

(define body-address (mk-wordaddress->address body-wordaddress))


(define (wordaddress-peek wordaddress) ;; returns the bytes of the whole word at given position
  (if (##fixnum? wordaddress)
      (let ((v (##make-u8vector word-size)))
	(##c-code
	 "
*((___WORD*)___BODY(___ARG1)) = *((___WORD*)(___INT(___ARG2)*___WS));
___RESULT=___VOID;"
	 v
	 wordaddress)
	v)
      (error "not a fixnum:" wordaddress)))

;; Can't use the ___HEADER macro, since this only works for tSUBTYPED, not for pairs.


;; =============================================================================

;; still also for word addresses, or so (dangerously undefined!, and
;; actually always assumes 4-byte words, even on 64 bit architectures,
;; right?)

(define-typed (copy-to-body@! v #(fixnum? wordaddr) #(fixnum? lenbytes))
  (##c-code "
char* body= ___CAST(char*, ___BODY(___ARG1));
char* p= ___CAST(char*, ___ARG2);
size_t lenbytes= ___INT(___ARG3);
memcpy(body, p, lenbytes);
" v wordaddr lenbytes)
  (void))

(define-typed (copy-from-body@! #(fixnum? wordaddr) v #(fixnum? lenbytes))
  (##c-code "
char* body= ___CAST(char*, ___BODY(___ARG1));
char* p= ___CAST(char*, ___ARG2);
size_t lenbytes= ___INT(___ARG3);
memcpy(p, body, lenbytes);
" v wordaddr lenbytes)
  (void))

(define (u32:wordaddr-peek wordaddr numwords)
  (let ((v ;; (make-u32vector (+ numwords 1) 111111)
	 (##make-u32vector (+ numwords 1))))
    (u32vector-set! v numwords       1777777777)
    (u32vector-set! v (dec numwords) 1234567880)
    (copy-to-body@! v wordaddr (* numwords 4))
    (assert (= (u32vector-ref v numwords) 1777777777))
    (assert (not (= (u32vector-ref v (dec numwords)) 1234567880)))
    (u32vector-shrink! v numwords)
    v))

;; (define (u8:wordaddr-peek wordaddr len)
;;   (let ((v (##make-u8vector (+ len 1))))
;;     (u8vector-set! v len 78)
;;     (u8vector-set! v (dec len) 42)
;;     (copy-to-body@! v wordaddr len)
;;     (assert (= (u8vector-ref v len) 78))
;;     (assert (not (= (u8vector-ref v (dec len)) 42)))
;;     (u8vector-shrink! v len)
;;     v))

(define (u8:wordaddr-peek wordaddr len)
  (let ((v (##make-u8vector len)))
    (copy-to-body@! v wordaddr len)
    v))

(define-typed (u8:wordaddr-poke wordaddr #(u8vector? v) #!optional len)
  ((typed-lambda (#(size0? len))
		 (copy-from-body@! wordaddr v len))
   (or len (u8vector-length v))))


;; =============================================================================

;; Finally proper bignum address based peek


(define @memcpy-to-object
  (c-lambda (scheme-object unsigned-int64 unsigned-int64)
	    int
	    "memcpy((void*)___BODY(___arg1), (void*)___arg2, (size_t)___arg3);"))

(define-typed (u8:peek addr #!optional (len 1))
  (let ((out (make-u8vector len 77)))
    ;; Still missing an address type, or even size_t. Just allow for
    ;; 64 bits, and all is fine? (Let it wrap around, even, fine?)
    (@memcpy-to-object out addr len)
    out))

(define-typed (u32:peek addr #!optional (len 1))
  (let ((out (make-u32vector len 77)))
    ;; Still missing an address type, or even size_t. Just allow for
    ;; 64 bits, and all is fine? (Let it wrap around, even, fine?)
    (@memcpy-to-object out addr (* len 4))
    out))

(define-typed (u64:peek addr #!optional (len 1))
  (let ((out (make-u64vector len 77)))
    ;; Still missing an address type, or even size_t. Just allow for
    ;; 64 bits, and all is fine? (Let it wrap around, even, fine?)
    (@memcpy-to-object out addr (* len 8))
    out))



;; =============================================================================


;; NOTE: for me, (subtype (cons 1 2)) returns 0, same as vector. All
;; other values are as listed in gambit.h. Probably pairs are really a
;; special case where the subtype is usually not correct.(?). I'm not
;; correcting this to ___sPAIR here, since my purpose is really
;; looking inside the data structures (and not a basis for type
;; dispatch).

(define (subtype obj) ;; is in the header
  (check-mem-allocated
   obj
   (thunk
    (##subtype obj))))



; (define (head-tag obj)
;   (if (mem-allocated? obj)
;       ))


(define (head-tag obj) ;; 3-bit tag, is in the header.
  (check-mem-allocated
   obj
   (thunk
    (##c-code "___RESULT= ___FIX(___HD_TYP(___HEADER(___ARG1)));" obj))))


; (define (deep-still-copy obj)
;   (let recur ((obj obj)
; 	      (cancel (thunk obj)))
;     (define (scancont)
;       )
;     (define (binarycont)
;       )
;     (define (choosecont)
;       (if (pair? obj)
; 	  scancont
; 	  (let ((st (subtype obj)))
; 	    (if (<= st 15)
; 		scancont
; 		binarycont))))
;     (define (scan obj cancel)
;       ((choosecont)))
;     (if (mem-allocated? obj)
; 	(case (head-tag obj)
; 	  ((0) ;; movable0
; 	   (cancel (scan obj cancel)))
; 	  ((1) ;; still
; 	   ;; ;;Ah  der übliche  callcc  basierte  nichtkopieren wenn nicht nötig ansatz?
; 	   (cancel
; 	    (call/cc    ;; HMM immer noch falsch:   wo welche cancelpunkte isch relevant
; 	     (lambda (newcancel)
; 	       (scan obj (thunk
; 			  (newcancel obj)))))))
; 	  ((3) ;; forw
; 	   (error "never seen, what to do with forwarded obj?"))
; 	  ((6) ;; perm
; 	   (cancel))
; 	  (else
; 	   (error "unknown (combined?) head tag:" (head-tag obj) obj)))
; 	(cancel))))
;^-todo finish.

;;#define ___HTB 3  number of head tag bits
;;" * note: the tag ___FORW+(1<<___TB) is also used"  ?
;;#define ___MOVABLE0 0
;;#define ___STILL    1
;;#define ___FORW     3
;;#define ___PERM     6

; (define (movable0? obj)
;   (check-mem-allocated
;    obj
;    (thunk
;     (##c-code "___RESULT= "))))


; (define (moved? obj)
;   (if (mem-allocated? obj)
;       (##c-code "___RESULT= 3333;")
;       (error "not a memory-allocated object:" obj)))


; (define (still? obj)
;   (and (mem-allocated? obj)
;        (##c-code "___RESULT= ___BOOLEAN ())))

(c-declare "
#define stillp(obj) (___HD_TYP(___HEADER(obj))==___STILL)
#define permp(obj) (___HD_TYP(___HEADER(obj))==___PERM)
#define is_still_or_perm(obj) (stillp(obj)||permp(obj))
")

(define (still-object? obj)
  (check-mem-allocated
   obj
   (thunk
    (##c-code "___RESULT=___BOOLEAN(stillp(___ARG1));" obj))))

; (define (mem-bytes obj)
;   (check-mem-allocated
;    obj
;    (thunk
;     (##c-code "___RESULT= ___FIX(___HD_BYTES(___HEADER(___ARG1)));" obj))))
;;what if it doesn't fit fixnums?real problem.

;;(define (mem-words obj) ;; hm but u8vectors for example really have a byte size.


(define (vector-like? obj);; pairs are returning #f for this, as do bignums,ratnums, ... basically it's for the mem-bytes check. 
  (##c-code "
___RESULT=___FAL;
if (___MEM_ALLOCATED (___ARG1) && !___PAIRP(___ARG1)) { /* really do have to check against pair */
    switch (___HD_TYP(___HEADER(___ARG1))) {
	case ___sVECTOR:
	case ___sSTRUCTURE:
	case ___sBOXVALUES:
	case ___sMEROON:
	case ___sSTRING:
	case ___sS8VECTOR:
	case ___sU8VECTOR:
	case ___sS16VECTOR:
	case ___sU16VECTOR:
	case ___sS32VECTOR:
	case ___sU32VECTOR:
	case ___sF32VECTOR:
	case ___sS64VECTOR:
	case ___sU64VECTOR:
	case ___sF64VECTOR:
	case ___sFLONUM:
		___RESULT= ___TRU;
    }
}
" obj))

(define (check-vector-like obj thunk)
  (if (vector-like? obj)
      (thunk)
      (error "not a vector-like object:" obj)))

(define (mem-bytes obj) ;; how many bytes is the object's body long?
  (check-vector-like
   obj
   (thunk
    ((c-lambda (scheme-object)
	       int
	       "___result= ___HD_BYTES(___HEADER(___arg1));")
     obj))))
; > (mem-bytes 1/3213213123223412412341234212312321412423123123123)
; 8
; hm interesting, why doesn't check-vector-like complain? But, the 8 bytes actually seem correct:
; > (##vector-ref 1/3213213123223412412341234212312321412423123123123 0)
; 1
; > (##vector-ref 1/3213213123223412412341234212312321412423123123123 1)
; 3213213123223412412341234212312321412423123123123
; it's like this :)


;; ATTENTION: make sure offsets and numbytes are fixnums!
(define (@vectorlike-bytecopy! to to-offset from from-offset numbytes)
  (##c-code "
char *to= ___CAST(char*,___BODY(___ARG1));
int to_offset= ___INT(___ARG2);
char *from= ___CAST(char*,___BODY(___ARG3));
int from_offset= ___INT(___ARG4);
int numbytes= ___INT(___ARG5);

memcpy(to+to_offset,from+from_offset,numbytes);
" to to-offset from from-offset numbytes)
  (void))

(define (@vectorlike-byteequal? to to-offset from from-offset numbytes)
  (##c-code "
char *to= ___CAST(char*,___BODY(___ARG1));
int to_offset= ___INT(___ARG2);
char *from= ___CAST(char*,___BODY(___ARG3));
int from_offset= ___INT(___ARG4);
int numbytes= ___INT(___ARG5);

___RESULT= ___BOOLEAN(0==memcmp(to+to_offset,from+from_offset,numbytes));
" to to-offset from from-offset numbytes))

;; well this is just ##u8vector-ref
; (define (@vectorlike-bytepeek obj offset)
;   (##c-code "
; char *obj= ___CAST(char*,___BODY(___ARG1));
; int offset= ___INT(___ARG2);

; ___RESULT= ___FIX(obj[offset]);
; " obj offset))


(define (@vectorlike-bytefill! obj offset value numbytes)
  (##c-code "
char *obj= ___CAST(char*,___BODY(___ARG1));
int offset= ___INT(___ARG2);
int value= ___INT(___ARG3);
int numbytes= ___INT(___ARG4);

memset(obj+offset,value,numbytes);
" obj offset value numbytes)
  (void))



(define (unsigned-fixnum? obj)
  (and (##fixnum? obj)
       (##fixnum.>= obj 0)))

(define (_mk-vectorlike-region-op unsafe-op)
  (lambda (to to-offset from from-offset numbytes)
    (if (and (vector-like? to)
	     (vector-like? from)
	     (unsigned-fixnum? to-offset)
	     (unsigned-fixnum? from-offset))
	(if (and (<= (+ to-offset numbytes) (mem-bytes to))
		 (<= (+ from-offset numbytes) (mem-bytes from)))
	    (unsafe-op to to-offset from from-offset numbytes)
	    (error "out of bounds access:" to to-offset from from-offset numbytes))
	(error "invalid types:" to to-offset from from-offset numbytes))))

;; dann doch wieder neue. doch wieder auf  check- basis gehen."?". oderdochnochnid?.
(define (_mk-vectorlike-ref-op unsafe-op)
  (lambda (obj offset)
    (if (and (vector-like? obj)
	     (unsigned-fixnum? offset))
	(if (<= offset (mem-bytes obj))
	    (unsafe-op obj offset)
	    (error "out of bounds access:" obj offset))
	(error "invalid types:" obj offset))))
(define (_mk-vectorlike-set-op unsafe-op)
  (lambda (obj offset value)
    (if (and (vector-like? obj)
	     (unsigned-fixnum? offset)
	     ;; hmm how to check the value? how to know the width? [todo].
	     )
	(if (<= offset (mem-bytes obj))
	    (unsafe-op obj offset value)
	    (error "out of bounds access:" obj offset value))
	(error "invalid types:" obj offset value))))

(define (_mk-vectorlike-fill-op unsafe-op)
  (lambda (obj offset value numbytes)
    (if (and (vector-like? obj)
	     (unsigned-fixnum? offset)
	     ;; hmm how to check the value? how to know the width? [todo].
	     (unsigned-fixnum? offset))
	(if (<= (+ offset numbytes) (mem-bytes obj))
	    (unsafe-op obj offset value numbytes)
	    (error "out of bounds access:" obj offset value numbytes))
	(error "invalid types:" obj offset value numbytes))))


(define vectorlike-bytecopy! (_mk-vectorlike-region-op @vectorlike-bytecopy!))
(define vectorlike-byteequal? (_mk-vectorlike-region-op @vectorlike-byteequal?))

(define vectorlike-byteref (_mk-vectorlike-ref-op ##u8vector-ref))
(define vectorlike-byteset! (_mk-vectorlike-set-op ##u8vector-set!))

(define vectorlike-bytefill! (_mk-vectorlike-fill-op @vectorlike-bytefill!))

(TEST
 > (define a "Hallo Welt")
 > (define b "Lechz ächz so oder so")
 > (begin (vectorlike-bytecopy! b 0 a 0 (* 4 (string-length a))) b)
 "Hallo Welt so oder so"
 > (%try-error (vectorlike-bytecopy! b 0 a 0 (+ (* 4 (string-length a)) 1)))
 #(error "out of bounds access:" "Hallo Welt so oder so" 0 "Hallo Welt" 0 41)
 > (%try-error (vectorlike-bytecopy! a 0 b 45 (+ (* 4 (string-length a)) 0)))
 #(error "out of bounds access:" "Hallo Welt" 0 "Hallo Welt so oder so" 45 40)
 > (begin (vectorlike-bytecopy! a 0 b 44 (- (* 4 (string-length a)) 4)) a)
 "so oder st"
 )

(TEST
 > (define a "Hallo Welt")
 > (define b "Lechz ächz so oder so")
 > (vectorlike-byteequal? a 0 b 0 4)
 #f
 > (vectorlike-byteequal? a (* 4 7) b 4 1)
 #t
 > (vectorlike-byteequal? a (* 4 7) b 4 4)
 #t
 > (vectorlike-byteequal? a 0 b 0 0)
 #t
 > (%try-error (vectorlike-byteequal? a 25 b 0 20))
 #(error "out of bounds access:" "Hallo Welt" 25 "Lechz ächz so oder so" 0 20))



(define (maybe-decompile v)
  (if (procedure? v)
      (let ((v* (##decompile v)))
	(and (not (procedure? v*))
	     v*))
      (error "not a procedure:" v)))


(define (decompile v)
  (or (maybe-decompile v)
      (error "can't decompile:" v)))


(define (maybe-procedure-name v)
  (if (procedure? v)
      (##procedure-name v)
      (error "not a procedure:" v)))

(define (repl)
  (##repl))

(define-typed (repl-within #(continuation? c) #!optional (arg1 ""))
  ;; don't know what arg1 is for
  (##repl-within c arg1))

(define-typed (continuation-location #(continuation? c))
  (##continuation-locat c))
