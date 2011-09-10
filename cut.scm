; REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
; ==========================================
;
; Sebastian.Egner@philips.com, 5-Jun-2002.
; adapted from the posting by Al Petrofsky <al@petrofsky.org>
; placed in the public domain
;
; The code to handle the variable argument case was originally
; proposed by Michael Sperber and has been adapted to the new
; syntax of the macro using an explicit rest-slot symbol. The
; code to evaluate the non-slots for cute has been proposed by
; Dale Jordan. The code to allow a slot for the procedure position
; and to process the macro using an internal macro is based on 
; a suggestion by Al Petrofsky. The code found below is, with
; exception of this header and some changes in variable names,
; entirely written by Al Petrofsky.
;
; compliance:
;   Scheme R5RS (including macros).
;
; loading this file into Scheme 48 0.57:
;   ,load cut.scm
;
; history of this file:
;   SE,  6-Feb-2002: initial version as 'curry' with ". <>" notation
;   SE, 14-Feb-2002: revised for <...>
;   SE, 27-Feb-2002: revised for 'cut'
;   SE, 03-Jun-2002: revised for proc-slot, cute
;   SE, 04-Jun-2002: rewritten with internal transformer (no "loop" pattern)
;   SE, 05-Jun-2002: replace my code by Al's; substituted "constant" etc.
;     to match the convention in the SRFI-document

; (srfi-26-internal-cut slot-names combination . se)
;   transformer used internally
;     slot-names  : the internal names of the slots
;     combination : procedure being specialized, followed by its arguments
;     se          : slots-or-exprs, the qualifiers of the macro

(define-macro* (srfi-26-internal-cut slot-names combination . se)
  (define (process-one-slot-or-expr)
    ;; process one slot-or-expr
    (match* se
	    ((nse . se)
	     (let ((positions combination))
	       (cond ((eq? (source-code nse) '<>)
		      (with-gensym
		       x
		       `(srfi-26-internal-cut (,@(source-code slot-names) ,x)
					      (,@(source-code positions) ,x)
					      ,@se)))
		     (else
		      `(srfi-26-internal-cut ,slot-names
					     (,@(source-code positions) ,nse)
					     ,@se)))))))
  
  (match* se
	  ;; construct fixed- or variable-arity procedure:
	  ;;   (begin proc) throws an error if proc is not an <expression>
	  (()
	   (match-list* combination
			((proc . args)
			 `(lambda ,slot-names ((begin ,proc) ,@args)))))
	  ((v)
	   (cond ((eq? (source-code v) '<...>)
		  (match-list* combination
			       ((proc . args)
				(with-gensym
				 rest-slot
				 `(lambda (,@(source-code slot-names) ,rest-slot)
				    (apply ,proc ,@args ,rest-slot))))))
		 (else
		  (process-one-slot-or-expr))))
	  (_
	   (process-one-slot-or-expr))))


; (srfi-26-internal-cute slot-names nse-bindings combination . se)
;   transformer used internally
;     slot-names     : the internal names of the slots
;     nse-bindings   : let-style bindings for the non-slot expressions.
;     combination    : procedure being specialized, followed by its arguments
;     se             : slots-or-exprs, the qualifiers of the macro

'(define-syntax srfi-26-internal-cute
  (syntax-rules (<> <...>)

    ;; If there are no slot-or-exprs to process, then:
    ;; construct a fixed-arity procedure,
    ((srfi-26-internal-cute
      (slot-name ...) nse-bindings (proc arg ...))
     (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
    ;; or a variable-arity procedure
    ((srfi-26-internal-cute
      (slot-name ...) nse-bindings (proc arg ...) <...>)
     (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))

    ;; otherwise, process one slot:
    ((srfi-26-internal-cute
      (slot-name ...)         nse-bindings  (position ...)   <>  . se)
     (srfi-26-internal-cute
      (slot-name ... x)       nse-bindings  (position ... x)     . se))
    ;; or one non-slot expression
    ((srfi-26-internal-cute
      slot-names              nse-bindings  (position ...)   nse . se)
     (srfi-26-internal-cute
      slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

; exported syntax

(define-macro* (cut . slots-or-exprs)
  `(srfi-26-internal-cut () () ,@slots-or-exprs))

'(define-macro* (cute . slots-or-exprs)
  `(srfi-26-internal-cute () () () ,@slots-or-exprs))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion cut a b c)
 (lambda () (a b c))
 > (expansion cut a b <> c)
 (lambda (GEN:526) (a b GEN:526 c))
 > (expansion cut a b <> <> c)
 (lambda (GEN:527 GEN:528) (a b GEN:527 GEN:528 c))
 > (expansion cut a b <> <>)
 (lambda (GEN:529 GEN:530) (a b GEN:529 GEN:530))
 > (expansion cut a b <> <> <...>)
 (lambda (GEN:531 GEN:532 GEN:533) (apply a b GEN:531 GEN:532 GEN:533))
 )