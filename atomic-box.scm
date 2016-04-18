;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

(export atomic-box
	atomic-box.value atomic-box.unbox
	atomic-box.mutex
	atomic-box.update!
	;; and the corresponding generics, .value, .mutex, .update!
	atomic-box-of
	)

(defstruct atomic-box
  constructor-name: _atomic-box
  value
  mutex)

(def (atomic-box-of pred)
     (lambda (v)
       (and (atomic-box? v)
	    (pred (atomic-box.value v)))))

(def. (atomic-box val)
  (_atomic-box val (make-mutex)))

(def. (atomic-box.update! b fn)
  (let-atomic-box ((v m) b)
		  (mutex-lock! m)
		  (with-exception-catcher
		   ;; everything is costly in this...
		   (lambda (e)
		     (mutex-unlock! m)
		     (raise e))
		   (& (letv ((v* res) (fn v))
			    (vector-set! b 1 v*) ;; hack
			    (mutex-unlock! m)
			    res)))))

(def. atomic-box.unbox
  atomic-box.value)

(def atomic-unbox atomic-box.value)

(TEST
 > (def b (atomic-box 1))
 > (atomic-box.update! b (lambda (v) (values (inc v) v)))
 1
 > (.value b)
 2
 > (.unbox b)
 2
 > (atomic-unbox b)
 2
 ;; XX test concurrency?..
 )

