;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test) ;; and implicitely cj-source

(TEST
 > (position* 10 11)
 #(position* 10 11)
 > (position*? (position* 10 11))
 #t
 > (position? (position* 10 11))
 #f
 > (position*? (position* 10 11))
 #t
 ;; hmm don't have the variant that accepts both yet
 ;; > (position?* (position* 10 11))
 ;; #t
 > (location* "foo" (position* 10 11))
 #(location* "foo" #(position* 10 11))
 > (location*? (location* "foo" (position* 10 #f)))
 #t
 > (location? (location* "foo" (position* 10 11)))
 #f
 ;; location?* is more lenient, allows objects that have the same API
 ;; on my side:
 > (location?* (location* "foo" (position* 10 11)))
 #t
 > (location?* (location* "foo" (position* 10 #f)))
 #f ;; because it's not usable for the location API

 > (location-string '#("/home/foo/lib/test.scm" 197230) omit-column?: #t)
 "\"/home/foo/lib/test.scm\"@623"
 > (location-string '#("lib/test.scm" 197230))
 "\"lib/test.scm\"@623.4"
 > (%try-error (location-string (location* "lib/test.scm" (position* 10 #f))))
 #(error
   "position-column: position* does not contain column:"
   #(position* 10 #f))
 > (location-string (location* "lib/test.scm" (position* 10 #f))
		    omit-column?: #t)
 "\"lib/test.scm\"@10"
 )

