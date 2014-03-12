;;; Copyright 2010 by Christian Jaeger <chrjae@gmail.com>

;;; This file is part of GIT System.
;;;
;;;    GIT System is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU Lesser General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    GIT System is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU Lesser General Public License for more details.
;;;
;;;    You should have received a copy of the GNU Lesser General Public License
;;;    along with GIT System.  If not, see <http://www.gnu.org/licenses/>.


;; C version of member?:

(define c-error-sym 'c-error-occurred)

(insert-result-of
 
;; variant for my (shortcutting, generic needing) representation:
(define (*C:tree-element C:element?)
  (lambda (v)
    ;;XXX eben how signal errors. hm. assert? wl jo?. wl grr.todo.  oder eben goto out und specialvalue.f-.
    ;; "if" ehr.
    (string-append
     "("(C:element? v)") ? "v" : "
     ;; und eben XXX derzeit unsafe simply mal:
     "___VECTORREF("v", ___FIX(1))")))

(define (*C:tree-left C:element?)
  (lambda (v)
    (string-append
     ;; XXX empty_tree var hardcoded. well. ~whynot~urg.
     "("(C:element? v)") ? empty_tree : "
     "___VECTORREF("v", ___FIX(3))")))
(define (*C:tree-right C:element?)
  (lambda (v)
    ;; all the XXX dito~
    (string-append
     "("(C:element? v)") ? empty_tree : "
     "___VECTORREF("v", ___FIX(4))")))

;; 'generic' part:

(define (member?-C-code C:lt C:element?)
  (define (C:empty-tree? v)
    (string-append "("v")==empty_tree"))
  (define C:tree-element (*C:tree-element C:element?))
  (define C:tree-left (*C:tree-left C:element?))
  (define C:tree-right (*C:tree-right C:element?))
  (string-append "
___SCMOBJ t= ___arg1;
___SCMOBJ x= ___arg2;
___SCMOBJ empty_tree= ___arg3;
___SCMOBJ c_error_sym= ___arg4; /*XXX use it!*/

tree_member_p: {
    if ("(C:empty-tree? "t")") {
        ___result=___FAL;
    } else {
        ___SCMOBJ v = "(C:tree-element "t")";
        if ("(C:lt "x" "v")") {
            t = "(C:tree-left "t")";
            goto tree_member_p;
        } else {
            if ("(C:lt "v" "x")") {
                t = "(C:tree-right "t")";
                goto tree_member_p;
            } else {
                ___result=___TRU;
            }
        }
    }
}
out:
"))

;; for number variant:   XXX only for fixnums!
(define (fixnum-C:lt a b)
  (string-append
   "___FIXLT("a", "b")"))

(define (fixnum-C:element? v)
  (string-append
   "___FIXNUMP(" v ")"))

(define (member?-schemecode name C:lt C:element?)
  `(define (,name t x)
     (let ((res ((c-lambda (scheme-object scheme-object scheme-object scheme-object)
			   scheme-object
			   ,(member?-C-code C:lt C:element?))
		 t x empty-tree c-error-sym)))
       (if (eq? res c-error-sym)
	   (raise c-error-sym)
	   res))))


`(begin
   ,(member?-schemecode 'tree:member?-fixnum fixnum-C:lt fixnum-C:element?)))

;; / C version.

(define* (tree:member? t x)
  (IF #t
      (begin ;; C variant
	(if (and (eq? (treeparameter-lt $param) <)
		 (eq? (treeparameter-element? $param) number?))
	    (tree:member?-fixnum t x)
	    (error "cannot do this C optimization for other than numbers (on top of that, silently only fixnums)")))
      (begin ;; Scheme variant
	(cond ((empty-tree? t)
	       #f)
	      (else
	       (let ((v (tree-element t)))
		 (if (lt x v)
		     (tree:member? (tree-left t) x)
		     (if (lt v x)
			 (tree:member? (tree-right t) x)
			 #t))))))))
