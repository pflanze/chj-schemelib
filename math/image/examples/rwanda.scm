;;; Copyright 2014-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (math/mapfn mapfn)
	 (math/image/visualize plot))

;;  	Total population (in thousands)) 	Population aged 0–14 (%) 	Population aged 15–64 (%) 	Population aged 65+ (%)
;; 1950 	2,072 	45.1 	52.3 	2.6
;; 1955 	2,386 	46.3 	50.8 	2.9
;; 1960 	2,771 	48.1 	49.1 	2.8
;; 1965 	3,221 	47.8 	49.5 	2.7
;; 1970 	3,749 	47.8 	49.7 	2.5
;; 1975 	4,390 	47.7 	49.9 	2.4
;; 1980 	5,179 	48.1 	49.7 	2.2
;; 1985 	6,081 	48.9 	49.1 	2.0
;; 1990 	7,110 	49.1 	48.7 	2.2
;; 1995 	5,570 	48.4 	49.3 	2.3
;; 2000 	8,098 	45.4 	52.0 	2.6
;; 2005 	9,202 	42.4 	55.0 	2.7
;; 2010 	10,624 	42.6 	54.7 	2.7

(defstruct population-point
  year
  ;; thousands:
  total
  ;; percentages:
  aged-0-14
  aged-15-64
  aged-65+)

(def population-data
     (map (C apply population-point _)
	  '((1950 	2072 	45.1 	52.3 	2.6)
	    (1955 	2386 	46.3 	50.8 	2.9)
	    (1960 	2771 	48.1 	49.1 	2.8)
	    (1965 	3221 	47.8 	49.5 	2.7)
	    (1970 	3749 	47.8 	49.7 	2.5)
	    (1975 	4390 	47.7 	49.9 	2.4)
	    (1980 	5179 	48.1 	49.7 	2.2)
	    (1985 	6081 	48.9 	49.1 	2.0)
	    (1990 	7110 	49.1 	48.7 	2.2)
	    (1995 	5570 	48.4 	49.3 	2.3)
	    (2000 	8098 	45.4 	52.0 	2.6)
	    (2005 	9202 	42.4 	55.0 	2.7)
	    (2010 	10624 	42.6 	54.7 	2.7))))


(def. (string.string x)
  x)

(def (show-population-data)
     (strings-join (map (lambda (p)
			  (let-population-point
			   ((year total a b c) p)
			   ;; XX add this to constructor instead
			   (assert (< 99 (+ a b c) 101))
			   (let ((percent (lambda (v)
					    (inexact.round-at
					     (* total (/ (- 100 v) 100))
					     2))))
			     (strings-join (map .string
						(list year
						      total
						      (percent c)
						      (percent (+ b c))
						      "\n"))
					   " "))))
			population-data)
		   ""))

(def population (mapfn (map (lambda (p)
			      (cons (.year p)
				    (.total p)))
			    population-data)))

(def (show-rwanda)
     (plot (list population zero/1) 1950 2010))

;; now for labels...
