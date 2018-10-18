;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require class
	 (cj-typed-1 (mutable cj-typed-1:error?)
		     (mutable cj-typed-1:.string)))

;; Base trait/interface for all exception/error values.

(definterface error

  ;; human-readable string representation that explains what the error
  ;; is about in a nice way that's presentable to a user, OK?
  (method (string s)))

;; Note that |error?| now doesn't return true for Gambit's own error
;; objects. Is that OK since Gambit only ever throws them, unlike the
;; purpose for |error?| ones?


;; let cj-typed know about us
(set! cj-typed-1:error? error?)
(set! cj-typed-1:.string .string)

