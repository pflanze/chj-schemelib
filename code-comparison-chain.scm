;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (list-util fold/fn0))

(export comparison-chain-<-littleendian-expand
	comparison-chain-<=-littleendian-expand
	comparison-chain-littleendian-expand
	#!optional
	a->a+b-fieldnames
	fold/fn0 ;; re-export
	code-comparison-chain:or-and/
	code-comparison-chain:op/)


(def (a->a+b-fieldnames fieldnames)
     (map (lambda (fn)
	    (values fn (gensym fn)))
	  fieldnames))


(def (code-comparison-chain:or-and/ <op =op)
     (lambda-values ((a-fieldname b-fieldname) tail)
	       `(or (,<op ,a-fieldname ,b-fieldname)
		    (and (,=op ,a-fieldname ,b-fieldname)
			 ,tail))))

(def (code-comparison-chain:op/ op)
     (lambda-values ((a b))
	       `(,op ,a ,b)))

(def (comparison-chain-littleendian-expand <op =op last-op)
     (C fold/fn0
	(code-comparison-chain:or-and/ <op =op)
	(code-comparison-chain:op/ last-op)
	_))


(def comparison-chain-<-littleendian-expand
     (comparison-chain-littleendian-expand `< `= `<))

(def comparison-chain-<=-littleendian-expand
     (comparison-chain-littleendian-expand `< `= `<=))


(TEST
 > (def fns (map (lambda (nam)
		   (values nam (symbol-append "b-" nam)))
		 '(sec
		   min
		   hour
		   mday
		   month-1
		   year-1900)))
 > (comparison-chain-<-littleendian-expand fns)
 (or (< year-1900 b-year-1900)
     (and (= year-1900 b-year-1900)
	  (or (< month-1 b-month-1)
	      (and (= month-1 b-month-1)
		   (or (< mday b-mday)
		       (and (= mday b-mday)
			    (or (< hour b-hour)
				(and (= hour b-hour)
				     (or (< min b-min)
					 (and (= min b-min)
					      (< sec b-sec)))))))))))

 > (comparison-chain-<=-littleendian-expand fns)
 (or (< year-1900 b-year-1900)
     (and (= year-1900 b-year-1900)
	  (or (< month-1 b-month-1)
	      (and (= month-1 b-month-1)
		   (or (< mday b-mday)
		       (and (= mday b-mday)
			    (or (< hour b-hour)
				(and (= hour b-hour)
				     (or (< min b-min)
					 (and (= min b-min)
					      (<= sec b-sec))))))))))))


