;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Reimplementation of some R5RS and SRFI-1 procedures as macros for
;; inlining. Sick?

(require easy
	 (list-util let-pair)
	 (cj-source-quasiquote quasiquote-source))

(export (macro %map)
	(macro %for-each %stream-for-each))

(defmacro (%map fn l0 . ls)
  (if (null? ls)
      (let ((code/F (lambda (F)
		      (with-gensyms
		       (REC L A R)
		       (quasiquote-source
			(let ,REC ((,L ,l0))
			     (if (null? ,L)
				 '()
				 (let-pair ((,A ,R) ,L)
					   (cons (,F ,A)
						 (,REC ,R))))))))))
	(if (symbol? (source-code fn))
	    (code/F fn)
	    (with-gensym
	     F
	     (quasiquote-source
	      (let ((,F ,fn))
		,(code/F F))))))
      (error "%map with multipla arguments not implemented yet")))


(def (%for-each-expand FV proc l)
     (let ((code/P (lambda (P)
		     (with-gensyms
		      (LP L A R)
		      (quasiquote-source
		       (let ,LP ((,L ,l))
			    (,FV (,L)
				 (when (not (null? ,L))
                                       (let-pair ((,A ,R) ,L)
                                                 (,P ,A)
                                                 (,LP ,R))))))))))
       (if (symbol? (source-code proc))
	   (code/P proc)
	   (with-gensym
	    P
	    (quasiquote-source
	     (let ((,P ,proc))
	       ,(code/P P)))))))

(defmacro (%for-each proc l)
  (%for-each-expand `no-FV proc l))

(defmacro (%stream-for-each proc l)
  (%for-each-expand `FV proc l))

