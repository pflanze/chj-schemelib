;; Copyright 2005-2017 by Christian Jaeger <ch@christianjaeger.ch>

;; Published unter the same terms as Gambit: dual LGPL
;; version 2.1 or Apache version 2.0 license

;; BUGS: The non-fast non-pretty versions (with the line-wrapping
;; inside tags) give blank page in chromium (related to js?).

;; ** NOTE: this version just includes all dependencies so that it
;; does not require chjmodule **.  It's not such a good idea modifying
;; code here. But if you need to do anyway, still feed the changes
;; upstream to Christian please.


;; http://okmij.org/ftp/Scheme/xml.html
;; https://en.wikipedia.org/wiki/SXML


(require easy
	 (srfi-1 last-pair append!)
	 cj-env	;; warn, for debgging  ah and now thunk
	 (cj-sxml sxml-element? sxml-begin? sxml-begin
		  with-sxml-element-attributes/else
		  sxml-element-attribute-ref
		  sxml-element:add-attributes-unless-present)
	 (stream stream-for-each)
	 (test TEST)
	 cj-warn
	 debuggable-promise)

(export sxml>>html-fast
	sxml>>xhtml-fast
	sxml>>xml-fast
	;; drop the -fast here, take those as the normal ones, OK?
	(method sxml.print-xhtml
		sxml.print-html
		sxml.print-xml)

	;; XX clean these up and not actually use as such?
	sxml>>xml-file
	sxml>>fast-xml-file
	sxml>>xhtml-file
	sxml>>fast-xhtml-file
	sxml>>html-file
	sxml>>fast-html-file
	
	sxml>>pretty-xml-file
	sxml>>pretty-xhtml-file

	sxml>>xhtml
	sxml>>html
	sxml>>xml
 
	sxml->html-string-fragment
	sxml->xml-string-fragment
	(method sxml.xml-string-fragment
		sxml.html-string-fragment)

	sxml->xml-string
	sxml->html-string
	sxml->xhtml-string
	(method sxml-element.xml-string
		sxml-element.html-string
		sxml-element.xhtml-string))


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)

(include "gambit-io-fast--include.scm")

(declare (not safe)) ;; XXX remove again!!!

(def unbound 'unbound)

(def (with-sxml-element/else elt cont-name-attrs-body
			     #!optional
			     (cont-else (lambda ()
					  (error "not an sxml element:" elt))))
     (if (pair? elt)
	 (let ((maybe-name (car elt)))
	   (if (symbol? maybe-name)
	       (let ((has-attrs (lambda (attrs rest)
				  (cont-name-attrs-body maybe-name attrs rest)))
		     (no-attrs (lambda (rest)
				 (cont-name-attrs-body maybe-name '(@) rest))))
		 (let ((maybe-2nd (cdr elt)))
		   (if (pair? maybe-2nd)
		       (let ((2nd-val (car maybe-2nd)))
			 (if (and (pair? 2nd-val)
				  (eq? (car 2nd-val)
				       '@))
			     (has-attrs 2nd-val
					(cdr maybe-2nd))
			     (no-attrs maybe-2nd)))
		       (no-attrs maybe-2nd))))
	       (cont-else)))
	 (cont-else)))

(def (with-sxml-element elt cont-name-attrs-body)
     (with-sxml-element/else elt cont-name-attrs-body))

(def (with-sxml-element-attributes/else element yes no)
     (with-sxml-element element
			(lambda (name attrs body)
			  (let ((alis (cdr attrs)))
			    (if (pair? alis)
				(yes attrs)
				(no))))))

(def (sxml-element-attribute-ref element #(symbol? attrname)
				 #!optional (missing unbound))
     (with-sxml-element-attributes/else
      element
      (lambda (attrs)
	(cond ((assoc attrname (cdr attrs))
	       => cdr)
	      (else #f)))
      (lambda ()
	(if (eq? missing unbound)
	    (error "missing sxml-attribute:" attrname)
	    missing))))


(def (maybe-sxml-element-attribute-alist element)
     (with-sxml-element-attributes/else element
					cdr
					(lambda ()
					  #f)))

(def (sxml-element:add-attributes-unless-present element alis)
     (let ((newattrs
	    (cons '@
	     (cond ((maybe-sxml-element-attribute-alist element)
		    => (lambda (present-alis)
			 (let lp ((alis alis)
				  (prepend '()))
			   (if (null? alis)
			       (begin
				 ;;(step)
				 (append! prepend present-alis))
			       (let* ((p (car alis))
				      (newkey (car p)))
				 (if (assoc newkey present-alis)
				     (lp (cdr alis)
					 prepend)
				     (lp (cdr alis)
					 (cons p prepend))))))))
		   (else
		    alis)))))
       ;; now check (todo: make namespaceetc aware!..) if attributes
       ;; are already present
       (let* ((nam (car element))
	      (r1 (cdr element))
	      (possibly-oldattrs (and (pair? r1)
				      (car r1)))
	      (have-oldattrs (and (pair? possibly-oldattrs)
				  (eq? '@ (car possibly-oldattrs))))
	      (body (if have-oldattrs
			(cdr r1)
			r1)))
	 (cons nam (cons newattrs body)))))


;; ----------------------------------------------------------------------


(def (@sxml-attributes l)
     ;; l must be an sxml-element; returns the *rest* of the attribute
     ;; list, or #f
     (let ((2ndpair (##cdr l)))
       (if (pair? 2ndpair)
	   (let ((2nd (##car 2ndpair)))
	     (if (and (pair? 2nd)
		      (eq? (##car 2nd) '@))
		 (##cdr 2nd)
		 #f))
	   #f)))

(def @string>> ##write-string)
(def (@char>> c port)
     (macro-write-char-neverlock c port))

(def (@symbol>> sym port)
     (@string>> (##symbol->string sym) port))

(def @car ##car)
(def @cdr ##cdr)
(def @cddr ##cddr)

(def (@attrlist>> attrlist port xml?)
     (cond ((pair? attrlist)
	    (let ((d (@car attrlist)))
	      (if (pair? d)
		  (let ((key (@car d))
			(val (@cdr d)))
		    (if (symbol? key)
			(begin
			  (let ((atom>>
				 (lambda (val)
				   (and val
					(begin
					  (@char>> #\space port)
					  (@symbol>> key port)
					  (@char>> #\= port)
					  (@char>> #\" port)
					  (let ((piece>>
						 (lambda (val)
						   (atom>>htmlquoted val port #f xml? #t))))
					    (cond ((sxml-begin? val)
						   (for-each piece>> (cdr val)))
						  ((pair? val)
						   (for-each piece>> val))
						  (else
						   (piece>> val))))
					  (@char>> #\" port))))))
			    (cond ((pair? val)
				   (atom>> (@car val)))
				  ((null? val)
				   ;; empty attribute value. ok for html.
				   (if xml?
				       (error "missing value for attribute:" key)
				       (begin
					 (@char>> #\space port)
					 (@symbol>> key port))))
				  (else
				   (atom>> val)))))
			;; error; or treat the whole thing like elements,
			;; e.g. accept listwraps? :
			(error "attribute key is not a symbol:" key)))
	       
		  ;; else something is strange.
		  ;;(error "attrlist-display: invalid attrlist:" attrlist)))
		  ;; no, accept it, like in '(img (@ (src
		  ;; "http://www.ethlife.ethz.ch/pix/favicon.png") (align
		  ;; "absbottom") (alt "") #f (border 0)))
		  ))
	    (@attrlist>> (@cdr attrlist) port xml?))
	   ((null? attrlist))
	   (else
	    (error "@attrlist>>: not a list:" attrlist))))


(TEST
 > (sxml->html-string-fragment '(p (@ (title (##begin 123 4 5 56)))))
 "<p title=\"1234556\">")


(def (@string>>htmlquoted str port count-chars? in-attributes?)
     (let ((len (##string-length str)))
       (let loop ((pos 0))
	 (if (##fixnum.< pos len)
	     (begin
	       (@char>>htmlquoted (##string-ref str pos)
				  port count-chars? in-attributes?)
	       (loop (##fixnum.+ pos 1)))))))
  

(def (html-whitespace? char)
     (case char
       ((#\space #\newline #\return #\tab #\page) #t)
       (else #f)))

(def (html-hyphenationchar? char)
     (case char
       ((#\- #\~ ;; todo weitere.
	     ) #t)
       (else #f)))

;; hmm.  basically everything that's not a word char is a word break.

;; XX proper unicode handling?...
(def (html-wordchar? char)
     (or (and (char>=? char #\a)
	      (char<=? char #\z))
	 (and (char>=? char #\A)
	      (char<=? char #\Z))
	 (case char
	   ((#\ä #\ö #\ü #\Ä #\Ö #\Ü #\Ç #\ç #\_ #\É #\é #\È #\è #\à #\À) #t)
	   (else #f))))

(def (@char>>htmlquoted char port count-chars? in-attributes?)
     ;;   (if count-chars?
     ;;       (if (html-wordchar? char)
     ;; 	  (serializeprocess-string-nextchar! port)
     ;;	  (serializeprocess-string-reset!)))
     (cond ((case char
	      ((#\<) "&lt;")
	      ((#\>) "&gt;")
	      ((#\&) "&amp;")
	      ((#\") (if in-attributes?
			 "&quot;"
			 (begin (@char>> char port) #f)))
	      (else
	       (@char>> char port)
	       #f))
	    =>(lambda(str)
		(@string>> str port)))))


(def (atom-or-list>>htmlquoted value
			       port
			       count-chars?
			       xml?
			       in-attributes?
			       maybe-level)
     (def subproc
	  (lambda(item)
	    (sxml>>fast item port xml? maybe-level)))
     (cond ((pair? value)
	    (for-each subproc value))
	   ((promise? value)
	    (stream-for-each subproc value))
	   (else
	    (atom>>htmlquoted value port count-chars? xml? in-attributes?))))


(def (atom>>htmlquoted atom port count-chars? xml? in-attributes?)
     (let self ((atom atom))
       (cond ((string? atom)
	      (@string>>htmlquoted atom port count-chars? in-attributes?))
	     ((char? atom)
	      (@char>>htmlquoted atom port count-chars? in-attributes?))
	     ((symbol? atom)
	      (@symbol>> atom port)
	      ;; I'm using this for outputting |&nbsp;| etc.; but maybe
	      ;; should use something else, for better error
	      ;; checking.(flattenedtags) ?
	   
	      ;; (serializeprocess-string-reset!)
	      )
	     ((null? atom)
	      #|nothing|#)
	     ((number? atom)
	      (##display atom port))
	     ((boolean? atom)
	      #|nothing|#)

	     ;; text (or more general: atom) lists or streams: (no
	     ;; subelements!)
	     ((pair? atom)
	      (for-each self atom))
	     ((promise? atom)
	      (stream-for-each self atom))

	     (else
	      (error "atom>>htmlquoted: unknown type of:" atom)))))


(def indentation-width 1)

(def (@sxml-element>> l port xml? maybe-level)
     ;;  (serializeprocess-string-reset!)
     (let* ((next-level (and maybe-level
			     (+ maybe-level indentation-width)))
	    (body->>> ;; proxy back to sxml>>fast
	     (lambda (body)
	       (& (for-each (lambda (item)
			      (sxml>>fast item port xml? next-level))
			    body))))
	    (nam (@car l))
	    (is-comment
	     ;; upcase version is the one shown in the reference
	     (or (eq? nam '*COMMENT*)
		 (eq? nam '*comment*))))
       (if is-comment
	   (@string>> "<!--" port)
	   (begin 
	     (@char>> #\< port)
	     (@symbol>> nam port)))
       (let ((content (cond ((@sxml-attributes l)
			     ;; XX what should happen with *COMMENT*
			     ;; elements having attributes? being handled
			     ;; strangely currently, probably.
			     =>(lambda(attrlist)
				 (@attrlist>> attrlist port xml?)
				 (@cddr l)))
			    (else (@cdr l)))))
	 (if is-comment
	     (cond ((pair? content)
		    ((body->>> content))
		    (@string>> "-->" port))
		   ((null? content)
		    (@string>> "-->" port))
		   (else
		    (error "@sxml-element>>: improper list: " l)))
	     (let* ((maybe-indent>> (lambda (maybe-level)
				      (if maybe-level
					  (begin
					    (@char>> #\newline port)
					    (let lp ((i maybe-level))
					      (if (> i 0)
						  (begin
						    (@char>> #\space port)
						    (lp (- i 1)))))))))
		    (out>>/body>> (lambda (body>>)
				    (maybe-indent>> next-level)
				    (@char>> #\> port)
				    (body>>)
				    (@char>> #\< port)
				    (@char>> #\/ port)
				    (@symbol>> (@car l) port)
				    (maybe-indent>> maybe-level)
				    (@char>> #\> port)))
		    (end>> (&
			    (if xml? (begin
				       (@char>> #\space port)
				       (@char>> #\/ port)))
			    (@char>> #\> port))))
	       ;; XX use improper-for-each instead
	       (cond ((pair? content)
		      (out>>/body>> (body->>> content)))
		     ((null? content)
		      (end>>))
		     (else
		      (out>>/body>> (& (sxml>>fast content port xml? next-level)))
		      ;; (end>>) hm why do I not need this?
		      )))))))


;; item may be an atom, element, or list containing elements (tree).
(def (sxml>>fast item port xml? maybe-level)
     (if (and (port? port)
	      (or (not maybe-level)
		  (##fixnum? maybe-level)))
	 (@sxml>>fast item port xml? maybe-level)
	 (error "invalid input type(s):" port maybe-level)))


(def *warn-thunk-evaluation* #f)
(set! *warn-thunk-evaluation* #f)

(def (@sxml>>fast item port xml? maybe-level)
     (let self ((item item))
       ;;(paramdata (make-paramdata 0));;UNFINISHED! not threadsafe like this.

       ;; XX why is all this logic here; @sxml-element>> has it again?
       ;; rewrite really.
       (cond 
	((pair? item)
	 (cond ((sxml-begin? item)
		(for-each self (cdr item)))
	       ((sxml-element? item)
		(@sxml-element>> item port xml? maybe-level))
	       (else
		(for-each self item))))
	((vector? item)
	 (vector-for-each self item))
	((promise? item)
	 (let ((item (force item)))
	   (if (pair? item)
	       (stream-for-each self item)
	       (self item))))
	((null? item))
	((procedure? item)
	 (if *warn-thunk-evaluation*
	     (warn "evaluating procedure" item))
	 (@sxml>>fast (item) port xml? maybe-level))
	(else
	 (atom-or-list>>htmlquoted item port #t xml? #f maybe-level)))))


(def (sxml-element:add-attribute el attributes)
     ;; XX do it right? currently cheap and simple.
     `(,(car el)
       ,attributes
       ,@(cdr el)))


(def (sxml>>html* item
		  #!optional
		  (port (current-output-port))
		  maybe-level)
     (sxml>>fast item port #f (and maybe-level
				   (if (boolean? maybe-level)
				       0
				       maybe-level))))


(def (sxml>>xhtml* item
		   #!optional
		   (port (current-output-port))
		   maybe-level)
     (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" port)
     (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" port)
     (let ((lang (or (sxml-element-attribute-ref item 'xml:lang #f)
		     (sxml-element-attribute-ref item 'lang #f)
		     "en")))
       (sxml>>fast (sxml-element:add-attributes-unless-present
		    item
		    `((xmlns "http://www.w3.org/1999/xhtml")
		      (xml:lang ,lang)
		      (lang ,lang)))
		   port
		   #t
		   (and maybe-level
			(if (boolean? maybe-level)
			    0
			    maybe-level)))))


(def (sxml>>xml* item
		 #!optional
		 (port (current-output-port))
		 maybe-level)
     ;; XX find out which output encoding the port is using?
     (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" port)
     (sxml>>fast item port #t (and maybe-level
				   (if (boolean? maybe-level)
				       0
				       maybe-level))))


(def (make-sxml>> sxml>>* maybe-level)
     (lambda (item port)
       (sxml>>* item
		port
		maybe-level)))

(def (sxml>>html-fast [sxml-element? item]
		      #!optional (port (current-output-port)))
     ((make-sxml>> sxml>>html* #f) item port))

(def. sxml.print-html sxml>>html-fast)

(def (sxml>>html [sxml-element? item]
		 #!optional (port (current-output-port)))
     ((make-sxml>> sxml>>html* #t) item port))

(def (sxml>>xhtml-fast [sxml-element? item]
		       #!optional (port (current-output-port)))
     ((make-sxml>> sxml>>xhtml* #f) item port))

(def. sxml.print-xhtml sxml>>xhtml-fast)

(def (sxml>>xhtml [sxml-element? item]
		  #!optional (port (current-output-port)))
     ((make-sxml>> sxml>>xhtml* #t) item port))

(def (sxml>>xml-fast [sxml-element? item]
		     #!optional (port (current-output-port)))
     ((make-sxml>> sxml>>xml* #f) item port))

(def. sxml.print-xml sxml>>xml-fast)

(def (sxml>>xml [sxml-element? item]
		#!optional (port (current-output-port)))
     ((make-sxml>> sxml>>xml* #t) item port))


(def (sxml>>xml-file item path)
     (call-with-output-file path (cut sxml>>xml item <>)))

(def (sxml>>fast-xml-file item path)
     (call-with-output-file path (cut sxml>>xml-fast item <>)))

(def (sxml>>xhtml-file item path)
     (call-with-output-file path (cut sxml>>xhtml item <>)))

(def (sxml>>fast-xhtml-file item path)
     (call-with-output-file path (cut sxml>>xhtml-fast item <>)))

(def (sxml>>html-file item path)
     (call-with-output-file path (cut sxml>>html item <>)))

(def (sxml>>fast-html-file item path)
     (call-with-output-file path (cut sxml>>html-fast item <>)))


(def (pretty-filer serialize)
     (lambda (item outpath #!optional noblanks) ;; XX noblanks unused now
       (let ((tmppath (string-append outpath "~"
				     (number->string (random-integer 100000)))))
	 (let ((p (open-process
		   `(path: "xmllint"
			   arguments:
			   ("--output" ,tmppath
			    ,@(if noblanks
				  '()
				  '(
				    ;; "--format"
				    ;; breaks embedded perltidy output!
				    ))
			    ;;"--noblanks"
			    "-")
			   char-encoding: UTF-8
			   buffering: #t))))
	   (with-exception-catcher
	    (lambda (e)
	      (with-exception-catcher
	       (lambda (e2)
		 (warn "delete-file didn't succeed on: "tmppath))
	       (thunk
		(delete-file tmppath)
		(warn "successfully deleted " tmppath)))
	      (raise e))
	    (thunk
	     (serialize item p)
	     (close-output-port p)
	     (let ((r (process-status p)))
	       (if (= r 0)
		   (rename-file tmppath outpath)
		   (error "sxml>>pretty-..-file: xmllint gave exit status: "
			  r)))))))))

(def sxml>>pretty-xml-file (pretty-filer sxml>>xml-fast))
(def sxml>>pretty-xhtml-file (pretty-filer sxml>>xhtml-fast))


;; 'fragment' means, that it's output doesn't include a <?..?> line.
(def (sxml:fragment-stringifyer xml?)
     (lambda (item #!optional maybe-level)
       (let ((port (open-output-string '())))
	 (sxml>>fast item port xml? maybe-level)
	 (get-output-string port))))

(def sxml->html-string-fragment (sxml:fragment-stringifyer #f))
(def. sxml.html-string-fragment sxml->html-string-fragment)

(def sxml->xml-string-fragment (sxml:fragment-stringifyer #t))
(def. sxml.xml-string-fragment sxml->xml-string-fragment)

(TEST
 > (sxml->xml-string-fragment '(a (@ (href "ha")) . "welt"))
 "<a href=\"ha\">welt</a>"
 > (sxml->xml-string-fragment '(a (@ (href "ha")) "welt"))
 "<a href=\"ha\">welt</a>"
 > (sxml->xml-string-fragment '(a (@ (href . "ha")) "welt"))
 "<a href=\"ha\">welt</a>")


(def (sxml:stringifyer >>)
     (lambda (item)
       (let ((port (open-output-string '())))
	 (>> item port)
	 (get-output-string port))))


(def sxml->xml-string (sxml:stringifyer sxml>>xml-fast))
(def. sxml-element.xml-string sxml->xml-string)

(def sxml->html-string (sxml:stringifyer sxml>>html-fast))
(def. sxml-element.html-string sxml->html-string)

(def sxml->xhtml-string (sxml:stringifyer sxml>>xhtml-fast))
(def. sxml-element.xhtml-string sxml->xhtml-string)

(TEST
 > (def v '(##begin (a (@ (href "ha")) . "welt") (br)))
 > (map (C _ v) (list .xml-string-fragment
		      .html-string-fragment))
 ("<a href=\"ha\">welt</a><br />"
  "<a href=\"ha\">welt</a><br>")
 > (%try (.xml-string v))
 (exception
 text:
 "no method found for generic .xml-string for value: (##begin (a (@ (href \"ha\")) . \"welt\") (br))\n")
 > (%try (sxml-element.xml-string v))
 (exception
 text:
 "item does not match sxml-element?: (##begin (a (@ (href \"ha\")) . \"welt\") (br))\n")

 > (def v '(div (a (@ (href "ha")) . "welt") (br)))
 > (.xml-string v)
 "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<div><a href=\"ha\">welt</a><br /></div>"
 > (.xhtml-string v)
 "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<div xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><a href=\"ha\">welt</a><br /></div>"
 > (.html-string v)
 "<div><a href=\"ha\">welt</a><br></div>")


;; ---------------------------------------------------------
;; idea to offer some serializer helper
;; XX remove all of that again, right?

; Idee 1: refactor. Cleaner einhaken.
; Idee 2: stubs. drin lassen als gleichnamig quasi aber einfach emptymachen.  hm wie gleich alles aufsmal?.  Oder: wie kann man ein SET von  variablen  um lenken?.  im obigen code?.
;  Na: mal so. serializeprocess-string-reset! und eben.

;(define serializeprocess-string-reset! paramdata/chars-in-a-row-reset!);; kein param
;(define serializeprocess-string-nextchar! paramdata/chars-in-a-row-inc!);; port param heh.
; ps JA staun obiges geht sogar  obwohl pre definition  und som it auch  runtimig  eigtl nicht geht  aber  es geht.
(define serializeprocess-string-reset! (lambda()#f));; kein param
(define serializeprocess-string-nextchar! (lambda(port)#f));; port param heh.


(define chars-in-a-row-allowed 5)
;(define chars-in-a-row-allowed 50000)

(define-structure paramdata
  chars-in-a-row  ; integer, chars seen since the last element.
  )

(define paramdata (make-parameter #f))

; (define (paramdata-chars-in-a-row-add! n)
;   (...

; (define (paramdata/chars-in-a-row)
;   (paramdata-chars-in-a-row (paramdata)))

; (define (paramdata/chars-in-a-row-set! n)
;   (paramdata-chars-in-a-row-set! (paramdata) n))

(define (paramdata/chars-in-a-row-reset!)
  (paramdata-chars-in-a-row-set! (paramdata) 0))

(define (paramdata/chars-in-a-row-inc! port)
  (let* ((p (paramdata))
	 (chars-in-a-row (paramdata-chars-in-a-row p)))
    ;; now I could use references. and an incf which then deref-inkrements.
    (if (>= chars-in-a-row chars-in-a-row-allowed)
	(begin
	  ;;(@string>> "&shy;" port)
	  ;;(@string>> "&#173;" port)
	  ;;(@string>> "&#45;" port)
	  (@string>> (next-separator) port)
	  (paramdata-chars-in-a-row-set! p 0))
	(paramdata-chars-in-a-row-set! p (+ chars-in-a-row 1)))))


(define next-separator
  (let* ((vec (vector
	       ;"&shy;"  ;todo entities hum?. also nbsp 'sadly'.
	       "&#173;"
	       ;"&#45;"
	       ))
	 (pos 0)
	 (len (vector-length vec)))
      (lambda()
	(set! pos (+ pos 1))
	(if (>= pos len)
	    (set! pos 0))
	(vector-ref vec pos))))


