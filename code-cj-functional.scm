;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (scheme-meta constant-expr?))


(def. (source.=> e)
  `(=> ,@(let lp ((e e)
		  (forms '()))
	   (mcase e
		  ((either symbol? constant-expr?)
		   (cons e forms))
		  (`(`caller `obj . `rest)
		   (lp obj
		       (cons (if (and (null? rest)
				      (symbol? (source-code caller)))
				 caller
				 `(,caller ,@rest))
			     forms)))))))


(TEST
 > (=> (quote-source
	(.string (.drop-while
		  (.drop-while
		   (.drop-while
		    (.drop-while
		     (.list category)
		     char-digit?)
		    char-whitespace?)
		   char-dash?)
		  char-whitespace?)))
       .=>
       cj-desourcify)
 (=> category
     .list
     (.drop-while char-digit?)
     (.drop-while char-whitespace?)
     (.drop-while char-dash?)
     (.drop-while char-whitespace?)
     .string)
 > (source.=> 'foo)
 (=> foo)
 > (source.=> '(bar 10))
 (=> 10 bar)
 > (source.=> '(bar ((values inc) 10)))
 (=> 10 ((values inc)) bar)
 > (with-exception-catcher source-error-message
			   (& (source.=> (quote-source (bar (lp () 1))))))
 "no match, expecting: (either symbol? constant-expr?) | `(`caller `obj quasiquote rest)")


