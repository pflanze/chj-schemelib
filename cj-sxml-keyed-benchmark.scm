;;; Copyright 2005-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-sxml-keyed)

(define test-doc-keyed
  '(doc
    (description "MD5 cryptographic hash function")
    (desc "This module implements the interface to RSA's MD5 message
       digest algorithm (see also "
	  (a href: "http://www.faqs.org/rfcs/rfc1321.html" "RFC 1321")
	  " and "
	  (a href: "http://en.wikipedia.org/wiki/MD5" "Wikipedia")
	  ")")
    (function (starts md5-context)
	      (args (md5-context "A context object")
		    (return "undefined")))
    (table cellpadding: 0 cellspacing: 10 align: center
	   (tr valign: top
	       (td align: right "Hello")
	       (td (font face:"Helvetica,Courier" style:"some" align: center
			 "Blah" (b " and so") (u (b " and so"))))
	       (td 1234)
	       (td 1313))
	   (tr valign: bottom
	       (td align: left
		   rowspan: 4
		   "last row")))))

(define (test n)
  (*do-times (- n 1)
	     (keyed->sxml test-doc-keyed))
  (keyed->sxml test-doc-keyed))
