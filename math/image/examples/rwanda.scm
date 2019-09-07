;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

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

(defclass (population-point year
                            ;; thousands:
                            total
                            ;; percentages:
                            aged-0-14
                            aged-15-64
                            aged-65+)

  (defmethod (some-absolute-values _)
    ;; xx add this to constructor instead
    (assert (< 99.999
               (+ aged-0-14
                  aged-15-64
                  aged-65+)
               100.11))
    (let ((percent (lambda (v)
                     (inexact.round-at
                      (* total (/ (- 100 v) 100))
                      2))))
      (list year
            total
            (percent aged-65+)
            (percent (+ aged-15-64
                        aged-65+))))))

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


;; (def. (string.string x)
;;   x)

(def (show-population-data)
     (strings-join (map (=>* .some-absolute-values
                             (.map .string)
                             (strings-join " ")
                             (string-append " \n"))
                        population-data)
		   ""))

(TEST
 > (show-population-data)
 "1950 2072 2018.13 934.47 \n1955 2386 2316.81 1104.72 \n1960 2771 2693.41 1332.85 \n1965 3221 3134.03 1539.64 \n1970 3749 3655.28 1792.02 \n1975 4390 4284.64 2094.03 \n1980 5179 5065.06 2491.1 \n1985 6081 5959.38 2973.61 \n1990 7110 6953.58 3491.01 \n1995 5570 5441.89 2695.88 \n2000 8098 7887.45 3676.49 \n2005 9202 8953.55 3892.45 \n2010 10624 10337.15 4525.82 \n"
 )


(def (list-of-population-point.function l accessor)
     (mapfn (map (lambda (p)
                   (cons (.year p) (accessor p)))
                 l)))

(def (show-rwanda)
     ;; (plot (list population zero/1) 1950 2010)
     ;; or
     (plot (map (C list-of-population-point.function population-data _)
                (list .total
                      (=>* .aged-0-14 (* 100))
                      (=>* .aged-15-64 (* 100))
                      (=>* .aged-65+ (* 100))))
           1950 2010 y0: 0))

;; now for labels...
