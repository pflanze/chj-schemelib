;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Make |jclass| and |jinterface| forms also available as |defclass| and
;; |definterface|, respectively.

(require define-macro-star
	 jclass)


(define-macro* (definterface decl . forms)
  (jclass:perhaps-expand-in-context '(definterface expansion#definterface
				       jinterface expansion#jinterface)
				    '(defclass expansion#defclass
				       jclass expansion#jclass)
				    #t stx #f #f))

(define-macro* (defclass decl . forms)
  (jclass:perhaps-expand-in-context '(definterface expansion#definterface
				       jinterface expansion#jinterface)
				    '(defclass expansion#defclass
				       jclass expansion#jclass)
				    #t stx #f #t))

