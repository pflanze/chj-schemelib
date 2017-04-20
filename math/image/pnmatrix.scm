;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 posix/mmap
	 math/image/pnmatrix-compiled ;; math/image/pnmatrix-macros ;; pnmatrix:headsize  u1matrix ?
	 u8-parse
	 math/image/pnmatrix-base)


(declare (block)
	 (standard-bindings)
	 (extended-bindings))

;; PNM matrices

;; Matrices that work on mmap'ed files, and have headers that
;; make those that follow standard spec work directly as PBM/PGM/PPM files.

;; Types are layered:
;; * pnmatrixbasetype is defined by the width
;;   of storage per single value (bpc=bit per channel)
;; * pnmatrixtype is defined by the meaning as well as the used
;;   pnmatrixbasetype and number of channels used:
;;     CMYK vs. RGB, RGBA, color vs. gray (1 / 3 / 4 channels)

;; Accessors are defined by the pnmatrixtype so as to be
;; meaning-safe. pnmatrixtype is the type meant to be used by the
;; user. Good?

;; Re-uses pnminfo from pnm.scm (which contain a static pnmformat
;; value, too) to give a full spec of a particular file.

;; TODO: maxval correctly everywhere (i.e. might be for example 100
;; [CMYK?], still u8 as storage).






;; XX keep in sync with pbc.pnmatrix-constructor-name in
;; pnmatrix-macros.scm
(define (bpc.pnmatrix-constructor bpc)
  ;; currently bpc is sufficient to distinguish float from integer
  ;; formats
  (xcase bpc
	 ((1) u1matrix)
	 ((8) u8matrix)
	 ((16) u16matrix)
	 ((32) f32matrix)
	 ((64) f64matrix)))

(define pnmatrix?
  (either
   ;;u1matrix?
   u8matrix?
   u16matrix?
   f32matrix?
   f64matrix?))

(define pnmatrix-magic "pnmatrix.scm 1.0")
(define (pnmatrix-comment len)
  (let* ((start (string-append "# "pnmatrix-magic"; [internal info follows, don't move!: "))
	 (lenstart (string-length start))
	 (lenleft (- len lenstart 2)))
    (string-append start (string-multiply "." lenleft) "]\n")))

(define. (pnminfo.pnmatrix-head-string info)
  (let* ((format (.format info))
	 (nl "\n")
	 (beforecomment (string-append (.magic format)
				       nl))
	 (aftercomment (string-append (number->string (.width info))
				      " "
				      (number->string (.height info))
				      nl
				      (if (.explicit-maxval? format)
					  (string-append
					   (number->string (.maxval info))
					   nl)
					  "")))
	 (comment (pnmatrix-comment (- pnmatrix:headsize
				       (string-length beforecomment)
				       (string-length aftercomment)))))
    (string-append beforecomment comment aftercomment)))


(define (pnm:open path)
  (let* ((fd (posix:open path O_RDWR))
	 (p (fd->port fd 'RDONLY))
	 (info (read-pnminfo p))
	 (info (.name-set info path))
	 (format (.format info))
	 ;; XXX now find out whether it is mappable?
	 
	 (start (posix:mmap #f
			    (.filesize info)
			    (+ PROT_READ PROT_WRITE)
			    (+ MAP_SHARED
			       ;; MAP_POPULATE (since Linux 2.5.46) ?
			       )
			    fd
			    0)))
    (close-port p)
    ((bpc.pnmatrix-constructor (.bpc info))
     (.pnmatrixtype info)
     info
     (.height info)
     (.width info)
     (.channels format)
     start)))


(define (pnm:create path info)
  (let* ((format (.format info))
	 (size (.filesize info))
	 (fd (posix:open path (+ O_RDWR O_CREAT O_EXCL) #o666)))
    (posix:ftruncate fd size)
    (let* ((start (posix:mmap #f
			      size
			      (+ PROT_READ PROT_WRITE)
			      (+ MAP_SHARED
				 ;; MAP_POPULATE (since Linux 2.5.46) ?
				 )
			      fd
			      0)))
      ;;^^ copypaste
      (posix:close fd)
      (let* ((str (.pnmatrix-head-string info))
	     (v (string->u8vector str)))
	(u8:poke start v))
      ;; copypaste:_
      ((bpc.pnmatrix-constructor (.bpc info))
       (.pnmatrixtype info)
       info
       (.height info)
       (.width info)
       (.channels format)
       start))))


(define (_create magic #!optional bpv)
  (lambda (pathnosuffix s0 s1)
    (let* ((format (pnm:magic.format magic))
	   (path (string-append pathnosuffix "." (.suffix format)))
	   (maxval (if bpv
		       (dec (expt 2 bpv))
		       (.expected-maxval format))))
      (pnm:create path
		  (pnminfo path
			   format
			   #f
			   s1
			   s0
			   maxval)))))



(define pbm:create (_create "P4"))
(define pgm8:create (_create "P5" 8))
(define pgm16:create (_create "P5" 16))
(define ppm8:create (_create "P6" 8))
(define ppm16:create (_create "P6" 16))
(define PRGBA8:create (_create "PRGBA" 8))
(define PRGBA16:create (_create "PRGBAH" 16))
(define PCMYK8:create (_create "PCMYK" 8))
(define PCMYK16:create (_create "PCMYKH" 16))
;; (define PF32G:create (_create "PF32G"))
;; (define PF32RGB:create (_create "PF32RGB"))
;; (define PF64G:create (_create "PF64G"))
;; (define PF64RGB:create (_create "PF64RGB"))





;; Importing standard pnm files:

;; not put into pnm.scm as that one doesn't know about pnmatrix.

(define (u8:read-u8-integer p)
  (let ((v (read-u8-integer p)))
    (assert (and (>= v 0)
		 (< v 256)))
    v))


;; anonymous creation.
(define pnm-anonymous-basepath "/run/shm/anonpnm-")
(random-source-randomize! default-random-source) ;; evil? or dunno.

(define (_make-p_m create suffix)
  (lambda (name s0 s1)
    (assert (not (string-contains? name "/")))
    (let lp ((i 5))
      (let* ((name (string-append "/run/shm/"
				  name
				  "-"
				  (.string (random-integer 1000000))))
	     (path (string-append name 
				  suffix)))
	(if (file-exists? path)
	    (if (< i 0)
		(error "file exists:" path)
		(lp (dec i)))
	    ;; stupid to have to feed name; should I avoid appending the
	    ;; suffix really? I can ask the info here if I want? Well
	    ;; not here but here it's hard coded anyway.
	    (create name s0 s1))))))

(define make-pgm8 (_make-p_m pgm8:create ".pgm"))
(define make-ppm8 (_make-p_m ppm8:create ".ppm"))


;; partly COPY PASTE of Mr:read-pnm-body
(define (pgm8:read-pnm-body p info)
  (let* ((read-lum (if (.raw? (.format info))
		       ##read-u8
		       u8:read-u8-integer)))
    ((lambda (width height maxval)
       (let* ((range (inc maxval))
	      ;; only gray supported, ok?
	      (_ (assert (= (.channels (.format info)) 1)))
	      ;; only of 8bps, ok? since otherwise would need dispatch
	      ;; constructor. (make-pgm depth h w) ? ah but accessors
	      ;; are an issue, too, thus, really fixed.
	      (_ (assert (= (.bpc info) 8)))
	      (m (make-pgm8 "load" height width)))
	 (for..<
	  (row 0 height)
	  (for..< (col 0 width)
		  (let ((lum (read-lum p)))
		    (pgm8.set!@ m row col lum))))
	 (let ((next (read-u8 p)))
	   (assert (eof-object? next)))
	 m))
     (.width info)
     (.height info)
     (.maxval info))))

;; and COPY paste of above..
(define (ppm8:read-pnm-body p info)
  (let* ((read-lum (if (.raw? (.format info))
		       ##read-u8
		       u8:read-u8-integer)))
    ((lambda (width height maxval)
       (let* ((range (inc maxval))
	      ;; only gray supported, ok?
	      (_ (assert (= (.channels (.format info)) 3)))
	      ;; only of 8bps, ok? since otherwise would need dispatch
	      ;; constructor. (make-pgm depth h w) ? ah but accessors
	      ;; are an issue, too, thus, really fixed.
	      (_ (assert (= (.bpc info) 8)))
	      (m (make-ppm8 "load" height width)))
	 (for..<
	  (row 0 height)
	  (for..< (col 0 width)
		  (let* ((r (read-lum p))
			 (g (read-lum p))
			 (b (read-lum p)))
		    (ppm8.set!@ m row col r g b))))
	 (let ((next (read-u8 p)))
	   (assert (eof-object? next)))
	 m))
     (.width info)
     (.height info)
     (.maxval info))))

(define pgm8:load (_:read-pnm-file pgm8:read-pnm-body))
(define ppm8:load (_:read-pnm-file ppm8:read-pnm-body))


(define. (pnmatrix.imagesc v #!optional invert?)
  (assert (not invert?))
  (xxsystem "display" (.name (vectorlib:info v))))


(define. (pnmatrix.sizes m)
  (values (.size0 m)
	  (.size1 m)))
