(require easy
	 eol
	 csv-defaults
	 (predicates length-=)
	 jclass
	 stream
	 oo-lib-vector)

(export csv-file-stream
	(jclass csv-reader)
	(jinterface input-provider
		    (jclass file-input-provider)))


;; error reporting
(defclass (read-csv-error [(either string? port?) path-or-port]
			  [fixnum-natural0? lineno]
			  ;; message from the perl library
			  [string? message]))

;; location tracking
(defclass (csv-cell [(maybe string?) value]
		    [(either string? port?) path-or-port]
		    [fixnum-natural0? lineno]
		    [fixnum-natural0? colno]))


(def (_csv-port-stream port maybe-file-or-port #!optional (tail '()))
     (let lp ((lineno 0))
       (delay
	 (let ((line (read-line port)))
	   (if (eof-object? line)
	       (begin
		 (close-port port)
		 (assert (zero? (process-status port)))
		 tail)
	       (let ((vals-or-signal
		      (xone (call-with-input-string line read-all)))
		     (rest (lp (inc lineno))))
		 (xcond ((and (vector? vals-or-signal)
			      (> (vector-length vals-or-signal) 0))
			 (let ((signal vals-or-signal))
			   (xcase (vector-ref signal 0)
				  ((OK)
				   (if (null? (force rest))
				       tail
				       (error "read-csv bug: did get OK signal before end of output")))
				  ((ERROR)
				   (read-csv-error (vector-ref signal 1)
						   ;; ^ OK re SECURITY? alternative:
						   ;;(or maybe-file-or-port port)
						   (vector-ref signal 2)
						   (vector-ref signal 3))))))
			((ilist? vals-or-signal)
			 (let ((vals vals-or-signal))
			   (cons (if maybe-file-or-port
				     (map/iota (lambda (val colno)
						 (csv-cell val
							   maybe-file-or-port
							   lineno
							   colno))
					       vals)
				     vals)
				 rest))))))))))


(def (csv-file-stream path
		      #!key
		      ([char? sep-char] (current-csv-sep-char))
		      ([eol-name? eol] (current-csv-eol))
		      (tail '())
		      source?)
     (_csv-port-stream
      (open-input-process
       (list path: "lib/csv2sexpr"
	     arguments: (list path
			      "-"
			      (string sep-char)
			      (symbol.string eol))
	     char-encoding: 'UTF-8))
      (and source? path)
      tail))


;;XX lib (and lostontie?)
(def (send-file-strings inport outport)
     ;; XX well.
     (let ((str (read-line inport #f)))
       (display str outport)))


(def (csv-port-stream port
		      #!key
		      ([char? sep-char] (current-csv-sep-char))
		      ([eol-name? eol] (current-csv-eol))
		      (tail '())
		      maybe-source)
     (let ((p (open-process
	       (list path: "./csv2sexpr"
		     arguments: (list "-"
				      "-"
				      (string sep-char)
				      (symbol.string eol))
		     ;; WOW, would really need different encoding for
		     ;; input vs. output. If I wanted to process bytes
		     ;; inbetween; breaking down so much. But yeah,
		     ;; char-encoding when opening orig file is done,
		     ;; then we have strings, we write them here as
		     ;; UTF-8 and read them back as the same, so,
		     ;; actually fine here. "send-file-strings kinda"
		     ;; does the transcoding.
		     char-encoding: 'UTF-8
		     stdout-redirection: #t
		     stdin-redirection: #t))))
       (future
	(let lp ()
	  (send-file-strings port p)
	  (close-output-port p)
	  ;; XX btw TODO: check status, don't even do that in csv-file-stream!
	  (close-port port)))
       (_csv-port-stream p maybe-source tail)))



;; XX add more
(def char-encodings '(UTF-8))

(def (char-encoding? v)
     (and (memq v char-encodings) #t))


(jinterface input-provider

	    (method (open) -> input-port?)

	    (jclass (file-input-provider [path-string? path-string]
					 [char-encoding? char-encoding])

		    (def-method (open s)
		      (open-input-file (list path: path-string
					     char-encoding: char-encoding)))

		    ;; (def-method (close s port)
		    ;;   (close-port port)) unused
		    ))


;;XX lib
(def (filter/snd vals keep?s)
     (if (or (null? vals) (null? keep?s))
	 '()
	 (let-pair ((v vals) vals)
		   (let-pair ((k? keep?s) keep?s)
			     (let ((r (filter/snd vals keep?s)))
			       (if k?
				   (cons v r)
				   r))))))

(TEST
 > (filter/snd '(a b c) '(#f #t #f))
 (b)
 > (filter/snd '(a b c) '(#f #t #t))
 (b c)
 > (filter/snd '(a b c) '(#f #t))
 (b)
 > (filter/snd '(a b c) '(#f))
 ()
 > (filter/snd '(a b c) '(#t))
 (a)
 ;; XX hmm what about these, really??:
 > (filter/snd '(a b c) '(#f #t #t #f))
 (b c)
 > (filter/snd '(a b c) '(#f #t #t #t))
 (b c))


(jclass (csv-reader [input-provider? input-provider]
		    #!key
		    [char? sep-char]
		    [eol-name? eol]
		    [(maybe natural0?) maybe-head-skip]
		    [boolean? skip-last?]
		    [(maybe (list-of natural0?)) maybe-columns])

	(def-method (stream s)
	  (let* ((s (csv-port-stream (.open input-provider)
				     sep-char: sep-char
				     eol: eol))
		 (s (if maybe-head-skip
			(stream-drop s maybe-head-skip)
			s))
		 (s (if skip-last?
			(stream-butlast s)
			s))
		 (s (if maybe-columns
			(stream-map (lambda (row)
				      (map (let* ((row* (list.vector row))
						  (len (vector.length row*)))
					     (lambda (i)
					       (vector.ref row* i)))
					   maybe-columns))
				    s)
			s)))
	    s)))

