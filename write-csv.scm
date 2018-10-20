;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (oo-util-lazy iseq?)
	 U
	 eol
	 csv-defaults
	 (char-util char-alphanumeric+?)
	 test
	 ;;safe-fx
	 )

;; https://en.wikipedia.org/wiki/Comma-separated_values
;; https://www.ietf.org/rfc/rfc4180.txt


(export (method iseq.write-csv-file)
	#!optional
	sep-chars
	sep-char?
	write-csv:value->string
	csv-escape)

(def sep-chars '[#\; #\, #\:])

(def (sep-char? v)
     (and (char? v)
	  (case v
	    ((#\; #\, #\:) #t)
	    (else #f))))


;; https://www.codeproject.com/Articles/231582/Auto-detect-CSV-separator :

;; "Rules for writing CSV files are pretty simple:
;;     If value contains separator character or new line character or begins with a quote – enclose the value in quotes.
;;     If value is enclosed in quotes – any quote character contained in the value should be followed by an additional quote character.
;; "

;; but that may not be true, only do not escape char-alphanumeric+?;
;; also, escape empty strings.

(def (csv-escape* sep-char str)
     (declare (fixnum))

     (let ((strlen (string-length str)))
       ;; check whether it needs quoting, and if so, also count the
       ;; number of quotes (can be 0)
       (if-let ((num-quotes
		 (let lp ((i 0)
			  (need-quoting? #f)
			  (num-quotes 0))
		   (if (< i strlen)
		       (let ((c (string-ref str i)))
			 (cond ((char=? c #\")
				(lp (inc i)
				    #t
				    (inc num-quotes)))
			       ((or (char=? c sep-char)
				    (not (char-alphanumeric+? c)))
				(lp (inc i)
				    #t
				    num-quotes))
			       (else
				(lp (inc i)
				    need-quoting?
				    num-quotes))))
		       (and need-quoting? num-quotes)))))

	       (let* ((outlen (+ strlen num-quotes 2))
		      (out (make-string outlen #\")))
		 (let lp ((i 0)
			  (j 1))
		   (if (< i strlen)
		       (let ((c (string-ref str i)))
			 (if (char=? c #\")
			     (lp (inc i) (+ j 2))
			     (begin
			       (string-set! out j c)
			       (lp (inc i) (inc j)))))
		       (assert (= j (dec outlen)))))
		 out)
	       
	       (if (string-empty? str)
		   "\"\"" ;; make it different from #f which gives ""
		   str))))


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
 > (U csv-escape #\, 123/2)
 "\"123/2\"" ;; XX doubtful this will work?, also non-real numbers, and floating point formatting
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

;; XX should I always just create a type (value bundle) right away for
;; the options?! Are (openly placed) keywords even just a stupid idea?
;; Even in Perl you rather bundle them up in hashes?

;; (But then PG is very fond of them...)

(def (csv-row-writer #!key
		     [(maybe eol-name?) eol]
		     [(maybe char?) sep-char]
		     [output-port? port])
     (let ((eol (or eol (current-csv-output-eol)))
	   (sep-char (or sep-char (current-csv-output-sep-char))))
       (let ((eol* (list (eol-name.newline-string eol))))
	 (lambda (row)
	   (print port: port
		  (list-join (map (csv-escape sep-char)
				  row)
			     sep-char
			     eol*))))))


(def. (iseq.write-csv-file s
			   [path-string? path]
			   #!key
			   [(maybe eol-name?) eol]
			   [(maybe char?) sep-char]
			   ;; Not a boolean to avoid dependency on
			   ;; tempfile.scm and make it more flexible:
			   [(maybe procedure?) tempfile])
  (let* ((tmppath (if tempfile (tempfile path)
		      path))
	 (p (open-output-file tmppath))
	 (w (csv-row-writer port: p
			    eol: eol
			    sep-char: sep-char)))
    (let lp ((s s))
      (FV (s)
	  (if (null? s)
	      (begin
		(close-port p)
		(if tempfile
		    (rename-file tmppath path)))
	      (let-pair ((row s) s)
			(w row)
			(lp s)))))))

