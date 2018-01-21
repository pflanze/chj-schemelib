(require easy
	 U
	 eol
	 (char-util char-alphanumeric+?)
	 test)

;; https://en.wikipedia.org/wiki/Comma-separated_values
;; https://www.ietf.org/rfc/rfc4180.txt


(export stream->csv-file
	#!optional
	sep-chars
	sep-char?
	write-csv:value->string
	csv-escape)

(def sep-chars '#(#\; #\, #\:))

(def (sep-char? v)
     (and (char? v)
	  (case v
	    ((#\; #\, #\:) #t)
	    (else #f))))

(def (csv-escape* sep-char str)
     (if (string-any
	  ;; (lambda (c)
	  ;;   (or (char=? c sep-char)
	  ;; 	(char=? c #\")
	  ;; 	(char=? c #\newline)
	  ;; 	(char=? c #\return)))

	  ;; but, maybe, probably, that was too lenient, need more
	  ;; quotes to prevent errors:

	  (lambda (c)
	    (or (char=? c sep-char)
		(not (char-alphanumeric+? c))))
	  str)
	 ;; XX more efficient would be to count the number of #\"
	 ;; first (perhaps as part of the above), then.
	 (list->string
	  (cons #\"
		(fold-right (lambda (c rest)
			      (let ((r (cons c rest)))
				(if (char=? c #\")
				    (cons #\" r)
				    r)))
			    '(#\")
			    (string->list str))))
	 (if (string-empty? str)
	     "\"\"" ;; make it different from #f which gives ""
	     str)))


;; map a Scheme value to a string representation that's fine to use in
;; CSV; note that this is *not* the string that appears in the file
;; (i.e. it is not escaped yet)
(def (write-csv:value->string v) -> string?
     (cond ((string? v)
	    v)
	   ((number? v)
	    ;; XX number formats?
	    ;; XX and then treat just as a string, is this ok, correct?
	    (number->string v))
	   (else
	    (error "write-csv:value->string: don't know how to handle type of value:" v))))

(def ((csv-escape sep-char) v)
     (if v
	 (csv-escape* sep-char (write-csv:value->string v))
	 ""))


(TEST
 > (U csv-escape #\, "foobarn")
 "foobarn"
 > (U csv-escape #\, 123)
 "123"
 > (U csv-escape #\, "123")
 "123"
 > (U csv-escape #\, "123,2")
 "\"123,2\""
 > (U csv-escape #\, "123.2")
 "123.2"
 > (U csv-escape #\. "123.2")
 "\"123.2\""
 > (U csv-escape #\. ".")
 "\".\""
 > (U csv-escape #\; ".")
 "."
 > (U csv-escape #\; "12,3")
 "\"12,3\""
 > (U csv-escape #\; "12,3\n")
 "\"12,3\n\""
 ;; hm should the \n be escaped (represented as a literal) really?
 > (U csv-escape #\, "foo,barn")
 "\"foo,barn\""
 > (U csv-escape #\, "foo\nbarn")
 "\"foo\nbarn\""
 > (U csv-escape #\, "foo\r\nbarn")
 "\"foo\r\nbarn\""
 > (U csv-escape #\, "foo\r\n,barn")
 "\"foo\r\n,barn\""
 > ((csv-escape #\,) "foobar")
 "foobar"
 > ((csv-escape #\,) "foo\"bar")
 "\"foo\"\"bar\""
 > ((csv-escape #\,) "foo\"")
 "\"foo\"\"\""
 )


(def (csv-row-writer #!key
		     #(eol-name? eol)
		     #(char? sep-char)
		     #(output-port? port))
     (let ((eol* (list (eol-name.string eol))))
       (lambda (row)
	 (print port: port
		(list-join (map (csv-escape sep-char)
				row)
			   sep-char
			   eol*)))))


(def (stream->csv-file s
		       #(path-string? path)
		       #!key
		       #(eol-name? eol)
		       #(char? sep-char))
     (let* ((p (open-output-file path))
	    (w (csv-row-writer port: p
			       eol: eol
			       sep-char: sep-char)))
       (let lp ((s s))
	 (FV (s)
	     (if (null? s)
		 (close-port p)
		 (let-pair ((row s) s)
			   (w row)
			   (lp s)))))))

;; XX should I always just create a type (value bundle) right away for
;; the options?! Are (openly placed) keywords even just a stupid idea?
;; Even in Perl you rather bundle them up in hashes?

;; (But then PG is very fond of them...)

