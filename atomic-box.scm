;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

(export (struct atomic-box)
	(method atomic-box.update!)
	(method atomic-box.unbox) atomic-unbox
	atomic-box-of
	#!optional
	_atomic-box)

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

;; fn: (A?) -> (values A? B?)
(def. (atomic-box.update! b fn) ;; -> B?
  (let-atomic-box
   ((v m) b)
   (dynamic-wind
       (lambda ()
	 (mutex-lock! m))
       (lambda ()
	 (letv ((v* res) (fn v))
	       (vector-set! b 1 v*) ;; hack
	       res))
       (lambda ()
	 (mutex-unlock! m)))))

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

