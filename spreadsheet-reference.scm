(require easy
	 class
	 char-util
	 parse1
	 test)

(export (class spreadsheet-reference-absolute)
	(class spreadsheet-reference-relative)
	excel-col-char-list.natural
	string.spreadsheet-reference-absolute
	string.spreadsheet-reference
	#!optional
	parse-formula:reference
	natural.excel-col-list
	natural.excel-col-string
	take-until)


(def (take-until pred l)
     (take-while (complement pred) l))

(def integer-char? (either (C char=? _ #\-)
			   char-digit?))

(def. ilist.number (=>* char-list.string string.number))



(defclass ((spreadsheet-reference #f) [(maybe string?) sheet-name])

  (defclass (spreadsheet-reference-absolute [natural? row]
					    [natural? col])

    (defmethod (excel-formula-string-fragment s)
      ;; but this one is not used in XML but user interface, only
      (string-append (if sheet-name (string-append sheet-name "!")
			 "")
		     (.excel-col-string col)
		     (number.string row)))

    (defmethod (relative-to a b)
      (let-spreadsheet-reference-absolute
       ((sheet-name row1 col1) b)

       (spreadsheet-reference-relative sheet-name
				       (- row1 row)
				       (- col1 col)))))

  (defclass (spreadsheet-reference-relative [integer? row]
					    [integer? col])
		
    (defmethod (excel-formula-string-fragment s)
      (string-append (if sheet-name (string-append sheet-name "!")
			 "")
		     (if (zero? row) "R"
			 (string-append "R[" (number.string row) "]"))
		     (if (zero? col) "C"
			 (string-append "C[" (number.string col) "]"))))

    (defmethod (absolute s [spreadsheet-reference-absolute? t])
      (let-spreadsheet-reference-absolute
       ((_ r0 c0) t)
       (spreadsheet-reference-absolute sheet-name
				       (+ r0 row)
				       (+ c0 col))))))

(def (excel-col-char-list.natural l)
     (let lp ((l l)
	      (res 0))
       (if (null? l)
	   res
	   (let-pair ((a l*) l)
		     (let ((d (inc (- (char.integer a) (char.integer #\A)))))
		       (assert (<= 1 d 26))
		       (lp l*
			   (+ (* res 26) d)))))))

(TEST
 > (excel-col-char-list.natural '(#\A))
 1
 > (excel-col-char-list.natural '(#\B))
 2
 > (%try-error (excel-col-char-list.natural '(#\b)))
 [error "assertment failure: (<= 1 d 26)" (<= 1 34 26)]
 > (excel-col-char-list.natural '(#\Z))
 26
 > (excel-col-char-list.natural '(#\A #\A))
 27
 > (excel-col-char-list.natural '(#\A #\B))
 28
 > (excel-col-char-list.natural '(#\A #\Z))
 52
 > (excel-col-char-list.natural '(#\B #\A))
 53
 > (excel-col-char-list.natural '(#\C #\B))
 80
 > (excel-col-char-list.natural '(#\H #\Y))
 233
 > (excel-col-char-list.natural '(#\A #\C #\H))
 762)


(def. (natural.excel-col-list n)
  (let lp ((res '())
	   (n n))
    (if (zero? n)
	res
	(letv ((q m) (quotient+modulo (dec n) 26))
	      (lp (cons (integer.char (+ (char.integer #\A) m))
			res)
		  q)))))

(TEST
 > (natural.excel-col-list 1)
 (#\A)
 > (natural.excel-col-list 2)
 (#\B)
 > (natural.excel-col-list 26)
 (#\Z)
 > (natural.excel-col-list 27)
 (#\A #\A)
 > (natural.excel-col-list 28)
 (#\A #\B)
 > (natural.excel-col-list 80)
 (#\C #\B)
 > (.excel-col-list 762)
 (#\A #\C #\H))

(def. natural.excel-col-string
  (comp char-list.string natural.excel-col-list))


;; "$IDs.C40"
(def. (string.spreadsheet-reference-absolute s)
  (let* ((l (.list s))
	 (name (take-until (C char=? _ #\!) l))
	 (l (drop l (inc (length name))))
	 (col (take-while char-alpha? l))
	 (l (drop l (length col))))
    (spreadsheet-reference-absolute (.string name)
				    (string.natural (.string l))
				    (excel-col-char-list.natural col))))


(def parse-formula:reference
     (PARSE1
      (mlet ((maybe-name (maybe-capture-until (C char=? _ #\!) #t)))
	    (match-string "R")
	    (mlet* ((openbracket? (match-string? "["))
		    (row (if openbracket?
			     (mlet ((row (capture-while integer-char?)))
				   (match-string "]")
				   ;; XX turn exceptions from number
				   ;; conversion into parse failure?
				   (return (.number row)))
			     (return 0)))
		    (col (mdo (match-string "C[")
			      (mlet ((col (capture-while integer-char?)))
				    (match-string "]")
				    (return (.number col))))))

		   (return
		    (spreadsheet-reference-relative (and maybe-name
							 (.string maybe-name))
						    row
						    col))))))

(def. (string.spreadsheet-reference str)
  (letv ((v rem) (parse-formula:reference (.list str)))
	(assert (null? rem))
	v))


(TEST
 > (def r (.spreadsheet-reference "IDs!R[-20]C[1]"))
 > r
 [(spreadsheet-reference-relative) "IDs" -20 1]
 > (.excel-formula-string-fragment r)
 "IDs!R[-20]C[1]"
 > (.absolute r (spreadsheet-reference-absolute "Anode DFMEA" 29 7))
 [(spreadsheet-reference-absolute) "IDs" 9 8]
 > (.col #)
 8

 ;; The formula showing up within Excel as "=IDs!C40" in sheet 11, B22:
 ;; (XX and in LibreOffice as IDs.C40, todo)
 > (.absolute (.spreadsheet-reference "IDs!R[18]C[1]")
	      (.spreadsheet-reference-absolute "Anode DFMEA!B22"))
 [(spreadsheet-reference-absolute) "IDs" 40 3]
 > (.excel-formula-string-fragment #)
 "IDs!C40" ;; and not "IDs!R[40]C[3]", which would be relative...

 > (.excel-formula-string-fragment
    (.relative-to (.spreadsheet-reference-absolute "IDs!C40")
		  (.spreadsheet-reference-absolute "IDs!C40")))
 "IDs!RC" ;; self-reference, probably invalid?
 > (.excel-formula-string-fragment
    (.relative-to (.spreadsheet-reference-absolute "IDs!C40")
		  (.spreadsheet-reference-absolute "Foo!B42")))
 "Foo!R[2]C[-1]"


 ;; various formats:
 > (def p (comp .show .spreadsheet-reference))
 > (p "IDs!R[-20]C[1]")
 (spreadsheet-reference-relative "IDs" -20 1)
 > (p "R[-20]C[1]")
 (spreadsheet-reference-relative #f -20 1)
 > (p "RC[1]")
 (spreadsheet-reference-relative #f 0 1)
 ;; > (p "R[5]C")  ?
 ;; (spreadsheet-reference-relative #f 5 0)
 )

