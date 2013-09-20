; cj Mon, 02 Jan 2006 22:09:52 +0100
; die "eigentlichen" utils... weil nie irgend wo richtig da ?.

;(set! *stream-strict* #t)
;(declare (not safe) (fixnum))

; wo sind die accessor functions scho wieder ?
; sxml-util sxml-tree-trans sxml-serializer sxml-to-xml cj-sxml-util

; -rw-rw-r--  1 chris chris 141 2005-05-01 22:06 sxml-util-module.scm
;  sxml-get
;  sxml-set!
; ^- oh noch gar nicht lauffähig, sxpath dependency nicht erfüllt.
; -rw-rw-r--  1 chris chris 120 2005-06-12 19:10 sxml-tree-trans-module.scm
;   SRV:send-reply
;   post-order pre-post-order replace-range
; -rw-rw-r--  1 chris chris 150 2005-06-12 19:13 sxml-to-xml-module.scm
;  sxml->xml
;  sxml->xml+
;  entag
;  enattr
;  string->goodHTML
; -rw-rw-r--  1 chris chris 208 2005-06-12 19:13 sxml-serializer-module.scm
;  SXML->HTML
;  sxml->html ;copy
;  entag
;  enattr
;  string->goodHTML

;  sxml->html-fast
;  sxml->xhtml-fast
;  sxml->xml-fast

; ^- und der hat auch  sxml-tag? sxml-attributes@  (sxml-attributes@ l) ;; l must be an sxml-tag;

; -rw-rw-r--  1 chris chris 225 2005-08-03 04:08 cj-sxml-util-module.scm
;  keyed->sxml
;  sxml->keyed


;; von sxml-serializer.scm: (aber sxml-tag? in sxml-element? umgenannt)
(define (sxml-element? l)
  (and (##pair? l)
       (##symbol? (##car l))))


(define (sxml-element-name element)
  (with-sxml-element element
		     (lambda (name attrs body)
		       name)))

(define (sxml-element-body element)
  (with-sxml-element element
		     (lambda (name attrs body)
		       body)))

; (define (sxml-attributes@ l) ;; l must be an sxml-element; returns the *rest* of the attribute list, or #f
;   (let ((2ndpair (##cdr l)))
;     (if (##pair? 2ndpair)
;         (let ((2nd (##car 2ndpair)))
;           (if (and (##pair? 2nd)
;                    (eq? (##car 2nd) '@))
;               (##cdr 2nd)
;               #f))
;         #f)))
;aber, es ist blöd, das pair mit @ weg zu schmeissen wenn mans dann doch wieder davor macht.
;also eher spezielle accessors machen !

(define (@with-sxml-element-attributes/else element yes no)
  (let ((2ndpair (cdr element)))
    (if (pair? 2ndpair)
        (let ((2nd (car 2ndpair)))
          (if (and (pair? 2nd)
                   (eq? (car 2nd) '@))
              (yes 2nd)
              (no)))
        (no))));;; +-unsafe!

(define (with-sxml-element-attributes/else element yes no)
  (with-sxml-element element
		     (lambda (name attrs body)
		       (let ((alis (cdr attrs)))
			 (if (pair? alis)
			     ;;(yes alis)
			     (yes attrs);; Wed, 31 May 2006 12:13:12 +0200: "attributes" soll MIT @ sein. hatt ich doch mal entschlossen. und untige funcs sind sonst falsch.
			     (no))))))

(define (with-sxml-element-attributes element yes)
  (with-sxml-element-attributes/else
   element
   yes
   (lambda ()
     ; (yes #f)
     '(@);; damit eben gleichbehandlung.  eh komisch eigentlich, zuerst oben @ wegnehmen zum schauen und dann geben wir doch wieder eins. ach chaos, (todo Wed, 31 May 2006 12:14:15 +0200).
     )));; könnte eigentlich gradsogut simplen return machen statt cps. eben, vgl unten.
;;; UND erwartet man nicht exception in diesem fall ?  ja eh doch.


(define (maybe-sxml-element-attribute-alist element)
  (with-sxml-element-attributes/else element
				     cdr
				     (lambda ()
				       #f)))
;; sollte ich das maybe- nennen ?  ja.

(define (sxml-element-attributes element)
  (with-sxml-element-attributes/else element
				     (lambda (v)
				       v)
				     (lambda ()
				       #f)))


;; Wed, 31 May 2006 12:19:09 +0200 (für/von cj-sxml-serializer)
(define (sxml-element:add-attributes-unless-present element alis)
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
    ;; now check (todo: make namespaceetc aware!..) if attributes are already present
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


(define (with-sxml-element/else elt
				cont-name-attrs-body
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

(define (with-sxml-element elt cont-name-attrs-body)
  (with-sxml-element/else elt cont-name-attrs-body))


(define unbound (gensym))
;; Wed, 31 May 2006 12:30:42 +0200: changed to act like table-ref: (it was using #f before as missing indicator)
(define (sxml-element-attribute-ref element attrname #!optional (missing unbound)) ;; attrname must be a symbol
  (with-sxml-element-attributes/else element
				     (lambda (attrs)
				       (cond ((assoc attrname (cdr attrs))
					      => cdr)
					     (else #f)))
				     (lambda ()
				       (if (eq? missing unbound)
					   (error "missing sxml-attribute:" attrname)
					   missing))))

;;      (v- ps Wed, 14 Jun 2006 22:48:44 +0200: sollte sxml-attributes:ref heissen wohl? grosser namens cleanup noch hängig)
(define (sxml-attribute-ref attributes attrname) ;; attrname must be a symbol
  (if (eq? '@ (car attributes))
      (cond ((assoc attrname (cdr attributes))
	     => cdr)
	    (else #f))
      (error "expected sxml-attributes, got:" attributes attrname)))

(define (sxml-attribute-value-ref attributes attrname) ;; attrname must be a symbol
  (cond ((assoc attrname (cdr attributes))
	 => cadr)
	(else #f)))
;;^- hm mix max: 2 things different!.

;; Wed, 14 Jun 2006 22:59:46 +0200: hm oder DIESE hier nennen wie oben gesagt. dies besser wie sol.
(define (sxml-attributes:ref attrs namesym #!optional (missing unbound))
  (if (and (pair? attrs)
	   (eq? '@ (car attrs)))
      (cond ((assoc namesym (cdr attrs))
	     => cadr);;(btw can this give exceptions? todo fix it better?)
	    (else
	     (if (eq? missing unbound)
		 (error "missing attribute named" namesym)
		 missing)))
      (error "expected sxml-attributes, got:" attrs namesym)))
;(ob ich nich schon mal nochwasandres irgendwohab?)



(define (sxml-element-bodytext element)
  ;; merge all text elements in the body to one string. croak if an element is there.
  ;; HE YUCK: I probably nowhere deal with lists of things correctly. todo.
  ;; and #f and #!void and such ? and single chars and and and. todo. probably string ports only rescue?
  ;;~hack solution:
  (apply string-append (sxml-element-body element)))


(define (sxml-element-search-subelement-with-name/attribute/value element eltname attrname attrvalue #!optional (tail '()))
  (with-sxml-element element
		     (lambda (name attrs body)
		       (stream-map/filter/tail
			(lambda (v yes no)
			  (with-sxml-element/else
			   v
			   (lambda (name attrs body)
			     (if (and (eq? name eltname)
				      (string=? (sxml-attribute-value-ref attrs attrname)
						attrvalue))
				 (yes v) ;; (hm, ich will ja gar keine map sondern nur filter ? !)
				 (no)))
			   no))
			tail
			body))))

;; urgh, tun untiges ist zu nah an obigem.
(define (sxml-element-search-subelement-with-name element eltname);; only finds those unmittelbar inside  not recursively
  (with-sxml-element element
		     (lambda (name attrs body)
		       (stream-map/filter (lambda (v yes no)
				     (with-sxml-element/else
				      v
				      (lambda (name attrs body)
					(if (eq? name eltname)
					    (yes v) ;;(warum nahm ich map/filter wenn filter allein schon genüge wäre ? hm wegen dem no on error? könnt ich false geben.)
					    (no)))
				      no))
				   body))))

; (define (sxml-element-search-subelement-with-pathlist elements pathlist)
;   (if (null? pathlist)
;       elements
;       (let ((fro (car pathlist))
; 	    (bac (cdr pathlist)))
; 	(stream-map (lambda (ele)
; 		      (sxml-element-search-subelement-with-pathlist ele bac))
; 		    (stream-map (lambda (eleme)
; 				  (sxml-element-search-subelement-with-name eleme fro))
; 				elements)))))
;      (sxml-element-search-subelement-with-pathlist (sxml-element-search-subelement-with-name element (car pathlist)) (cdr pathlist))))

; (define (sxml-elements-match-pathlist elements pathlist) ; cont)
;   (if (null? pathlist)
; 					;(cont elements)
;       elements
;       (let ((fro (car pathlist))
; 	    (bac (cdr pathlist)))
; 	(stream-map (lambda (eleme)
; 		      (sxml-elements-match-pathlist
; 		       (sxml-element-search-subelement-with-name eleme fro)
; 		       bac))
; 		    elements))))


(define (sxml-elements-match-subpathlist elements path tail)
					;   (warn "sxml-elements-match-pathlist:" elements path tail)
					;   (warn "                            :"  path)
					;   (warn "                            :" tail)
  (if (null? path)
      (begin
	;;(warn "elements,tail=" elements tail)
	(append elements tail) ;; Frage wegen * vs .
	)
      (let ((eltname (car path))
	    (rest (cdr path)))
	(fold-right
	 (lambda (v tail)
	   ;;(warn "X:" v tail)
	   (sxml-elements-match-subpathlist
	    (sxml-element-search-subelement-with-name v eltname)
	    
	    rest
	    tail))
	 elements
	 tail))))

(define (sxml-element-match-pathlist element path)
  (sxml-elements-match-subpathlist (list element)
				   path '()))



;; -- todo: move to cj-listutil: -------

;;" x- als so sicherstellende dinger .?.
;; oder für xtract oder so. hehe "


;;XX merge with |xone|
(define (x-list-one-value lis)
  (let ((lis (force lis)))
    (if (pair? lis)
	(if (null? (force (cdr lis)))
	    (car lis)
	    (error "x-list-one-value: too many values:" lis))
	(if (null? lis)
	    (error "x-list-one-value: got empty list")
	    (error "x-list-one-value: not a list:" lis)))))

(define (x-list-maybe-one-value lis)
  (let ((lis (force lis)))
    (if (pair? lis)
	(if (null? (force (cdr lis)))
	    (car lis)
	    (error "x-list-maybe-one-value: too many values:" lis))
	(if (null? lis)
	    #f
	    (error "x-list-maybe-one-value: not a list:" lis)))))

;; --  string related routines, todo move parts to cj-string-util --------                      

(define (string-all-whitespace? str)
  (let ((len (string-length str)))
    (let iter ((pos 0))
      (if (= pos len)
	  #t
	  (cond ((char-whitespace? (string-ref str pos))
		 (iter (+ pos 1)))
		(else #f))))))


(define (sxml-whitespace? obj)
  (and (string? obj)
       (string-all-whitespace? obj)))
;(Jan  3 04:40 war mtime;) Mon, 13 Feb 2006 02:57:26 +0100: evtl. atom-whitespace? nennen?

(define (sxml-strip-whitespace doc)
  (with-sxml-element/else doc
			  (lambda (name attrs body)
			    `(,name
			      ,attrs
			      ,@ (stream-map/filter (lambda (v yes no)
					       (if (and (string? v)
							(string-all-whitespace? v))
						   (no)
						   (yes (sxml-strip-whitespace v))))
					     body)))
			  (lambda ()
			    (if (and (string? doc)
				     (string-all-whitespace? doc))
				#f
				doc))))


; (define (string-count-leading-chars string char-criterion)
;   (let ((len (string-length string)))
;     (let iter ((pos 0))
;       (if (= pos len)
; 	  pos
; 	  (if (char-criterion (string-ref string pos))
; 	      (iter (+ pos 1))
; 	      pos)))))

(define (string-count-chars//accessor string char-criterion accessor)
  (let ((len (string-length string)))
    (let iter ((pos 0))
      (if (= pos len)
	  pos
	  (if (char-criterion (accessor string len pos))
	      (iter (+ pos 1))
	      pos)))))
(define (string-count-leading-chars string char-criterion)
  (string-count-chars//accessor string char-criterion (lambda (str len pos)
							(string-ref str pos))))
(define (string-count-trailing-chars string char-criterion)
  (string-count-chars//accessor string char-criterion (lambda (str len pos)
							(string-ref str (- len pos 1)))))

(define (normalize-whitespace string)
  ;; equivalent of s/\s+/ /sg
  (let ((leadn (string-count-leading-chars string char-whitespace?))
	(trailn (string-count-trailing-chars string char-whitespace?))
	(len (string-length string)))
    (if (>= (+ leadn trailn)
	    len)
	;; string is only whitespace
	""
	;; else cound number of whitespace segments
	(let* ((endpos (- len trailn))
	       (redundantn (let iter ((pos leadn)
				      (i 0) ;;number of whitezones
				      (n 0)) ;;number of whitespaces
			     (if (= pos endpos)
				 (- n i)
				 (if (char-whitespace? (string-ref string pos))
				     (let iter2 ((pos (+ pos 1))
						 (n (+ n 1)))
				       (if (= pos endpos)
					   (error "BUG: ended in whitespace, can't be")
					   (if (char-whitespace? (string-ref string pos))
					       (iter2 (+ pos 1)
						      (+ n 1))
					       (iter (+ pos 1)
						     (+ i 1)
						     n))))
				     (iter (+ pos 1)
					   i
					   n))))))
	  (let* ((newlen (- len leadn trailn redundantn))
		 (newstr (##make-string newlen)))
	    (let iter ((oldpos leadn)
		       (newpos 0))
	      ;;(warn "iter: oldpos="oldpos ", newpos="newpos)
	      (if (= newpos newlen)
		  newstr
		  (let ((ch (string-ref string oldpos)))
		    (if (char-whitespace? ch)
			(begin
			  (string-set! newstr newpos #\space)
			  (let iter2 ((oldpos (+ oldpos 1)))
			    ;;(warn "  iter2: oldpos="oldpos ", newpos="newpos)
			    (if (= oldpos len) ;;will never be reached. theoretically.
				(error "BUG another one, right?")
				(let ((ch (string-ref string oldpos)))
				  ;;(warn "  iter2: ch=" ch)
				  (if (char-whitespace? ch)
				      (iter2 (+ oldpos 1))
				      (iter oldpos;;(+ oldpos 1) shit hatte ich eins zu viel  ich hätte eben hier doch nochmals set machen müssen  heil isch das n chaos
					    (+ newpos 1)))))))
			(begin
			  (string-set! newstr newpos ch)
			  (iter (+ oldpos 1)
				(+ newpos 1))))))))))))


(define nbsp #\xA0);; decimal 160.

;(define shy)  doch  was bringt es.

