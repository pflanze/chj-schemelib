(require easy)

(export comparison-chain-<-expand
	comparison-chain-<=-expand
	comparison-chain-expand)


(def (a->b-fieldnames fieldnames)
     (map gensym fieldnames))


(def (comparison-chain-expand <op =op last-op a-fieldnames b-fieldnames)
     (let rec ((a-fieldnames a-fieldnames)
	       (b-fieldnames b-fieldnames))
	 (let-pair
	  ((a-fieldname a-fieldnames*) a-fieldnames)
	  (let-pair
	   ((b-fieldname b-fieldnames*) b-fieldnames)

	   (if (null? (rest a-fieldnames))
	       `(,last-op ,a-fieldname ,b-fieldname)
	       `(or (,<op ,a-fieldname ,b-fieldname)
		    (and (,=op ,a-fieldname ,b-fieldname)
			 ,(rec a-fieldnames* b-fieldnames*))))))))

(def (comparison-chain-<-expand a-fieldnames b-fieldnames)
     (comparison-chain-expand `< `= `< a-fieldnames b-fieldnames))

(def (comparison-chain-<=-expand a-fieldnames b-fieldnames)
     (comparison-chain-expand `< `= `<= a-fieldnames b-fieldnames))


(TEST
 > (def fns (reverse '(sec
		       min
		       hour
		       mday
		       month-1
		       year-1900)))
 > (def b-fns (map (lambda (fn)
		     (symbol-append "b-" fn))
		   fns))
 > (comparison-chain-<-expand fns b-fns)
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

 > (comparison-chain-<=-expand fns b-fns)
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


