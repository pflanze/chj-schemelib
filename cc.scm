;; *simple* expression to C compiler

;; - no need for ctx, since, just any variable that isn't known to be fixed is a variable? (but, what about shadowing?)
;; - what about types? infer from ops in e ?
;; - hm and breaks basic block for rangechange? well for..< broke it anyway right?.

;; example:
;;  (CC (integer (fl* (fl- (Mr.ref@ m i0 i1) lo) rangechange)))
;; yielding something like:
;;  (##c-code "___SCMOBJ m=___ARG1; ___SCMOBJ i0=___ARG2; ....
;;  ___RESULT= ___FIX(___CAST(long, ((___F64VECTORREF(___VECTORREF(___VECTORREF(m,___FIX(3)),i0),i1) - lo) * rangechange)))
;; " m i0 i1 lo rangechange)

;; hm
;; > (expansion (Mr.ref@ m i0 i1))
;; ((lambda (m i0 i1) (Vr.ref@ (##vector-ref (##vector-ref m 3) i0) i1)) m i0 i1)
;; > Vr.ref@
;; #<procedure #2 ##f64vector-ref>
;; -> (##f64vector-ref (##vector-ref (##vector-ref m 3) i0) i1)
;;or keep intermediate variables ok?


;; (define-struct. ccop
;;   )

;; (define cc-opmap
;;   )

;; (define (cc e ctx)
;;   )


(define cc-map
  (list->table
   '(("(integer (fl* (fl- (Mr.ref@ m i0 i1) lo) rangechange))"
      .
      (##c-code "
 ___SCMOBJ m=___ARG1;
 ___SCMOBJ i0=___ARG2;
 ___SCMOBJ i1=___ARG3;
 ___SCMOBJ lo=___ARG4;
 ___SCMOBJ rangechange= ___ARG5;

 ___RESULT= ___FIX(___CAST(long, ((___F64VECTORREF(___VECTORREF(___VECTORREF(m,___FIX(3)),i0),i1) - ___FLONUM_VAL(lo)) * ___FLONUM_VAL(rangechange))));
" m i0 i1 lo rangechange)))))


(define-macro* (CC e)
  (or (table-ref cc-map (object->string (cj-desourcify e)) #f)
      (source-error e "missing manual compilation for expression")))


