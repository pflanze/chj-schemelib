(require easy
	 test)

(export english:numerize
	english:the-natural
	english:weekdays)

(include "cj-standarddeclares.scm")


(def (english:numerize singular-str #(natural0? n))
     (if (= n 1) singular-str
	 (string-append singular-str "s")))

(def (english:the-natural #(natural0? n))
     (let* ((s (.string n))
	    (len (.length s))
	    (c (string-ref s (dec len))))
       (string-append s
		      (if (< 10 n 20)
			  "th"
			  (case c
			    ((#\1) "st")
			    ((#\2) "nd")
			    ((#\3) "rd")
			    (else
			     "th"))))))

(TEST
 > (map english:the-natural (iota 25))
 ("0th"
  "1st"
  "2nd"
  "3rd"
  "4th"
  "5th"
  "6th"
  "7th"
  "8th"
  "9th"
  "10th"
  "11th"
  "12th"
  "13th"
  "14th"
  "15th"
  "16th"
  "17th"
  "18th"
  "19th"
  "20th"
  "21st"
  "22nd"
  "23rd"
  "24th")
 > (english:the-natural 31)
 "31st"
 > (english:the-natural 101)
 "101st"
 > (english:the-natural 203)
 "203rd")


(def english:weekdays
  '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
