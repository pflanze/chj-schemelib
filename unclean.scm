;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Functions returning uncleaned memory.

;; ***WARNING***: these are inherently UNSAFE and reveal secrets!


(require cj-typed
	 ;; test
	 (cj-env natural0?))

(export natural0-fixnum?
	make-unclean-string
	make-unclean-u8vector)


(define (natural0-fixnum? v)
  (and (fixnum? v)
       (natural0? v)))

(define-typed (make-unclean-string #(natural0-fixnum? len))
  (##make-string len))

(define-typed (make-unclean-u8vector #(natural0-fixnum? len))
  (##make-u8vector len))

