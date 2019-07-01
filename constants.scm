;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (define-macro-star)
	 (cj-phasing))

(both-times
 (define *CONSTs* (make-table)))

(define-macro* (CONST e)
  (let* ((sym (string->symbol (object->string (cj-desourcify e)))))
    (when (not (table-ref *CONSTs* sym #f))
          ;; heh location info from another place, then
          (table-set! *CONSTs* sym e))
    sym))

(define-macro* (CONSTANTS)
  (let ((e `(begin
	      ,@(map (lambda (sym.e)
		       `(define ,(car sym.e) ,(cdr sym.e)))
		     (table->list *CONSTs*)))))
    (set! *CONSTs* (make-table))
    e))

