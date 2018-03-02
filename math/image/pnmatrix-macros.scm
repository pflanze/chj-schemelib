;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (math/image/pnm pnm:magic.format
			 string-alist->fn)
	 )


(define pnmatrix:headsize 256)
;; could also be maxheadsize without ill effects? re mmap, anyway;
;; finding right place for counter etc. may be different; ehr, also,
;; finding start of body data should be fixed.



;; XX keep in sync with pnmformat.pnmatrix-constructor in
;; pnmatrix.scm; this one actually not based on pnmformat but instead
;; on bpc
(define (pbc.pnmatrix-constructor-name bpc)
  ;; currently bpc is sufficient to distinguish float from integer
  ;; formats
  (xcase bpc
	 ((1) 'u1matrix)
	 ((8) 'u8matrix)
	 ((16) 'u16matrix)
	 ((32) 'f32matrix)
	 ((64) 'f64matrix)))

(define (pbc.pnmatrix-values-constructor-name bpc)
  ;; currently bpc is sufficient to distinguish float from integer
  ;; formats
  (if (< bpc 32)
      ;; unsigned integer
      (symbol-append 'u (.string bpc) 'vector)
      ;; float
      (error "XX not impl yet")))


(define-struct. pnmatrixCcode
  bpc
  channels
  pointertype)


(define. (pnmatrixCcode.pnmatrix-val-pred v)
  (let ((bpc (.bpc v)))
    ;; currently bpc is sufficient to distinguish float from integer
    ;; formats
    (if (< bpc 32)
	;; unsigned integer
	(symbol-append 'u (.string bpc) '?)
	;; float
	(error "XX not impl yet"))))


(define (bpc*channels b c)
  (.append (.string b)
	   "*"
	   (.string c)))

(define. (pnmatrixCcode.key v)
  (bpc*channels (.bpc v)
		     (.channels v)))

(define nl "\n") ;;[hm remember |eos| or so in the compiler, including ";"]

(define. (pnmatrixCcode.base->data-addr@-code v)
  ;; receives: base addr
  (quasiquote-source
   (lambda (addr)
     (##c-code
      ,(.append
	(.pointertype v)"*m=
	  ___CAST("(.pointertype v)"*,
	     ___CAST(___U8*,___ARG1) + " (.string pnmatrix:headsize) ");"nl
	"___RESULT= ___CAST(___WORD, m);"nl)
      addr))))

(define. (pnmatrixCcode.dataaddr-ref@-code v)
  ;; receives: dataaddr, i
  (quasiquote-source
   (lambda (addr i)
     (##c-code
      ,(.append
	(.pointertype v)"*m= ___CAST("(.pointertype v)"*,___ARG1);"nl
	"size_t i= ___INT(___ARG2);"nl
	"___RESULT= ___FIX(m[i]);"nl)
      addr i))))

;; (define. (pnmatrixCcode.dataaddr-set!@-code v)
;;   ;; receives: dataaddr, i, v
;;   (quasiquote-source
;;    (lambda (addr i v)
;;      (##c-code
;;       ,(.append
;; 	(.pointertype v)"*m= ___CAST(void*, ___ARG1);" nl
;; 	"size_t i= ___INT(___ARG2);"nl
;; 	"int v= ___INT(___ARG3);"nl
;; 	"m[i]= v;"nl)
;;       addr i v)
;;      (void))))


;;much COPYPASTE sgh
(define. (pnmatrixCcode.set!@-code v)
  (let* ((channels (.string (.channels v)))
	 (args (map (compose-function gensym .string) (iota (.channels v)))))
    (quasiquote-source
     (lambda (m i0 i1 ,@args)
       (define s1 (,(symbol-append
		     '@
		     (pbc.pnmatrix-constructor-name (.bpc v))
		     '.size1)
		   m))
       (##c-code
	,(.append
	  (.pointertype v)"*m=
	  ___CAST("(.pointertype v)"*,
	     ___CAST(___U8*,___ARG1) + " (.string pnmatrix:headsize) ");"nl
	  "size_t s1= ___INT(___ARG2);"nl
	  "size_t i0= ___INT(___ARG3);"nl
	  "size_t i1= ___INT(___ARG4);"nl
	  (apply
	   .append
	   (map (lambda (argi)
		  (.append
		   (.pointertype v) " v"(.string argi)
		   "= ___INT(___ARG"(.string (+ argi 5)) ");"nl))
		(iota (.channels v))))
	  (.pointertype v)"*p= &(m[i0 * s1 * "channels" + i1 * "channels"]);"nl
	  (apply
	   .append
	   (map (lambda (argi)
		  (define i (.string argi))
		  (.append "p["i"]= v"i ";"nl))
		(iota (.channels v)))))
	(,(symbol-append '@ (pbc.pnmatrix-constructor-name (.bpc v))
			    '.addr) m) s1 i0 i1 ,@args)))))



(define bpc*channels.pnmatrixCcode
  (string-alist->fn
   "bpc*channels"
   (map (lambda_
	 (cons (.key _) _))
	(list
	 ;;(pnmatrixCcode 1 1 "___U8") egal for now
	 (pnmatrixCcode 8 1 "___U8")
	 (pnmatrixCcode 8 3 "___U8")
	 (pnmatrixCcode 8 4 "___U8")
	 (pnmatrixCcode 16 1 "___U16")
	 (pnmatrixCcode 16 3 "___U16")
	 (pnmatrixCcode 16 4 "___U16")
	 ;; and the floats? XXX
	 ))))
(define bpc+channels.pnmatrixCcode
  (compose-function bpc*channels.pnmatrixCcode
	   bpc*channels))

(both-times
 (define pnmatrixtypes
   ;; pnmatrixtype, magic, maybe-bpc (if not determined by format)
   '(
     ;; (pbm "P4" #f) no bpc*channels mapping yet
     (pgm8 "P5" 8)
     (ppm8 "P6" 8)
     (pgm16 "P5" 16)
     (ppm16 "P6" 16)
     (PRGBA8 "PRGBA" 8)
     (PRGBA16 "PRGBA" 16)
     ;; XX hmm these could share the layout of PRGBA*? except
     ;; restrictive type checking? but reuse underlying ones? dunno.:
     (PCMYK8 "PCMYK" 8)
     (PCMYK16 "PCMYK" 16)
     ;; (PF32G #f #f) no bpc*channels mapping yet
     ;; (PF32RGB #f #f)
     ;; (PF64G #f #f)
     ;; (PF64RGB #f #f)
     )))

(define (_define-pnmatrix-types)
  (quasiquote-source
   (begin
     ,@(map-apply (lambda (type magic maybe-bpc)
		    (let* ((format (pnm:magic.format magic))
			   (bpc (or maybe-bpc (.bpc format)))
			   (channels (.channels format))
			   (cgen (bpc+channels.pnmatrixCcode bpc channels))
			   (cs (iota channels))
			   (vars (map (compose-function gensym .string) cs))
			   (T (symbol-replace-_-with/ type))
			   (pnmatrixtype
			    ;;X grr another one of these? see pnminfo.pnmatrixtype
			    (car
			     (xone
			      (filter (mcase-lambda
			   	       (`(`pnmatrixtype `_magic `maybe-bpc)
			   		(and (string=? magic _magic)
			   		     (or (not maybe-bpc)
			   			 (= bpc maybe-bpc)))))
			   	      pnmatrixtypes))))
			   )
		      (quasiquote-source
		       (begin
			 ;; type predicate
			 (define (,(T '_?) v)
			   (and (,(symbol-append
				   (pbc.pnmatrix-constructor-name bpc) '?) v)
				(eq? (,(symbol-append
					'@
					(pbc.pnmatrix-constructor-name bpc)
					'.pnmatrixtype)
				      v)
				     ',pnmatrixtype)))
			 ;; ref
			 (define. ,(T '_.ref)
			   (typed-lambda
			    (#(,(T '_?) m) #(size0? i0) #(size0? i1)
			      #!optional cont)
			    (let* ((cont (or cont values))
				   (dataaddr
				    (,(.base->data-addr@-code cgen)
				     (,(symbol-append
					'@
					(pbc.pnmatrix-constructor-name bpc)
					'.addr)
				      m)))
				   (s1 (,(symbol-append
					  '@
					  (pbc.pnmatrix-constructor-name bpc)
					  '.size1)
					m))
				   (i (fx* (fx+ (fx* i0 s1) i1) ,channels)))
			      (cont ,@(map (lambda (c)
					     (quasiquote-source
					      (,(.dataaddr-ref@-code cgen)
					       dataaddr
					       (fx+ i ,c))))
					   cs)))))

			 (define-inline (,(T '_.ref@) m i0 i1 cont)
			   (declare (fixnum) (not safe))
			   (let* ((dataaddr
				   (,(.base->data-addr@-code cgen)
				    (,(symbol-append
				       '@
				       (pbc.pnmatrix-constructor-name bpc)
				       '.addr)
				     m)))
				  (s1 (,(symbol-append
					 '@
					 (pbc.pnmatrix-constructor-name bpc)
					 '.size1)
				       m))
				  (i (fx* (fx+ (fx* i0 s1) i1) ,channels)))
			     (cont ,@(map (lambda (c)
					    (quasiquote-source
					     (,(.dataaddr-ref@-code cgen)
					      dataaddr
					      (fx+ i ,c))))
					  cs))))
		       
		       ;;ç (define. ,(T '_.cref)
		       ;;   (typed-lambda
		       ;;    (#(,(T '_?) m) #(size0? i0) #(size0? i1) #(size0? c))
		       ;;    (assert (fx< c ,channels))
		       ;;    (let ((addr (,(.ref-addr@-code cgen) m i0 i1)))
		       ;;      (,(.addr-ref@-code cgen) addr c))))

		       ;;ç (define-inline (,(T '_.cref@) m i0 i1 c)
		       ;;   (declare (not safe))
		       ;;   (let ((addr (,(.ref-addr@-code cgen) m i0 i1)))
		       ;;     (,(.addr-ref@-code cgen) addr c)))
		    
		       ;; set!
		       (define. ,(T '_.set!)
			 (typed-lambda
			  (#(,(T '_?) m) #(size0? i0) #(size0? i1)
			    ,@(map (lambda (var)
				     (quasiquote-source
				      #(,(.pnmatrix-val-pred cgen) ,var)))
				   vars))
			  (,(.set!@-code cgen) m i0 i1 ,@vars)))

		       (define-inline (,(T '_.set!@) m i0 i1 ,@vars)
			 (declare (not safe))
			 (,(.set!@-code cgen) m i0 i1 ,@vars))

		       ;;ç (define. ,(T '_.cset!)
		       ;;   (typed-lambda
		       ;;    (#(,(T '_?) m) #(size0? i0) #(size0? i1) #(size0? c)
		       ;;      #(,(.pnmatrix-val-pred cgen) v))
		       ;;    (assert (fx< c ,channels))
		       ;;    (let ((addr (,(.ref-addr@-code cgen) m i0 i1)))
		       ;;      (,(.addr-set!@-code cgen) addr c v))))

		       ;;ç (define-inline (,(T '_.cset!@) m i0 i1 c v)
		       ;;   (declare (not safe))
		       ;;   (let ((addr (,(.ref-addr@-code cgen) m i0 i1)))
		       ;;     (,(.addr-set!@-code cgen) addr c v)))

		       ))))
     pnmatrixtypes))))


(define-macro* (define-pnmatrix-types)
  (_define-pnmatrix-types))

