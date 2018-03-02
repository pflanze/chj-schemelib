;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; instead of old |safe-string->symbol| or perfect hashtable stuff

(require easy predicates define-module cj-phasing test)


(def list-of-symbol? (list-of symbol?))

(def (make-list-of-symbol.token-table sym.key)
     (lambda (symbols)
       (list->table (map (lambda (sym)
			   ;; doubly type-checked?
			   (assert* symbol? sym
				    (lambda (sym)
				      (cons (sym.key sym) sym))))
			 symbols))))

(def. list-of-symbol.token-table
  (make-list-of-symbol.token-table .string))

(def. list-of-symbol.u8vector-token-table
  (make-list-of-symbol.token-table (comp-function .u8vector .string)))

(both-times ;; so that module-import in the same file works
 (define-module (<token-table> prefix token? list-of-symbol.token-table)
   (export token-table token-map)
  
   (def (token-table . symbols)
	(list-of-symbol.token-table symbols))

   (def (token-map . symbols)
	(let ((t (list-of-symbol.token-table symbols)))
	  (lambda (#(token? str))
	    (table-ref t str #f))))))

(module-import || <token-table> string? list-of-symbol.token-table)
(module-import |u8vector-| <token-table> u8vector? list-of-symbol.u8vector-token-table)

(TEST
 > (def m (token-map 'a 'b 'foo))
 > (def u8m (u8vector-token-map 'a 'b 'foo))
 > (map m '("a" "c" "" "foo"))
 (a #f #f foo)
 > (%try-error (m 'foo))
 #(error "str does not match token?:" foo) ;; XX hmm. Heh. getting interesting. 'Syntax evil'
 > (%try-error (u8m "a"))
 #(error "str does not match token?:" "a")
 > (map (comp-function u8m .u8vector) '("a" "c" "" "foo"))
 (a #f #f foo))

