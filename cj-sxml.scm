(require (cj-stream stream-map/filter stream-map stream-filter
		    stream-map/filter/tail)
	 ;;(cj-stream *stream-strict*)
	 (srfi-1 append! fold-right))

(export maybe-sxml-element-attribute-alist

	sxml-attribute-ref
	sxml-attribute-value-ref
	sxml-attributes:ref
	sxml-element-attribute-ref
	sxml-element-attributes
	sxml-element-name
	sxml-element-body
	sxml-element-bodytext
	sxml-element-match-pathlist
	sxml-element-search-subelement-with-name
	sxml-element-search-subelement-with-name/attribute/value
	sxml-element:add-attributes-unless-present
	sxml-element?
	sxml-elements-match-subpathlist
	sxml-strip-whitespace
	sxml-whitespace?

	with-sxml-element
	with-sxml-element-attributes
	with-sxml-element-attributes/else
	with-sxml-element/else

	nbsp

	#!optional ;; on request

	x-list-maybe-one-value
	x-list-one-value
	@with-sxml-element-attributes/else

	;;?
	unbound
	;;sollten raus?:
	char-whitespace?
	normalize-whitespace
	string-all-whitespace?
	string-count-chars//accessor
	string-count-leading-chars
	string-count-trailing-chars)


(define (sxml-element name atts body)
  (cons name (if (and atts (not (null? atts)))
		 (cons (cons '@ atts) body)
		 body)))

(TEST
 > (sxml-element 'foo #f (list "foo"))
 (foo "foo")
 > (sxml-element 'foo '() (list "foo"))
 (foo "foo")
 > (sxml-element 'foo '((baz "ha")) (list "foo"))
 (foo (@ (baz "ha")) "foo")
 )

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

(define (sxml-element-body-fold fn tail e)
  (with-sxml-element e
		     (lambda (name atts body)
		       ;; flatten body?
		       (sxml-element name atts (fold fn tail body)))))

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
			     (yes attrs)
			     (no))))))

(define (with-sxml-element-attributes element yes)
  (with-sxml-element-attributes/else
   element
   yes
   (lambda ()
     ;; messy, re-adding it, for equal treatment
     '(@))))

(define (maybe-sxml-element-attribute-alist element)
  (with-sxml-element-attributes/else element
				     cdr
				     (lambda ()
				       #f)))

(define (sxml-element-attributes element)
  (with-sxml-element-attributes/else element
				     (lambda (v)
				       v)
				     (lambda ()
				       #f)))


(define (sxml-element:add-attributes-unless-present element alis)
  (let ((newattrs
	 (cons '@
	  (cond ((maybe-sxml-element-attribute-alist element)
		 => (lambda (present-alis)
		      (let lp ((alis alis)
			       (prepend '()))
			(if (null? alis)
			    (begin
			      (append! prepend present-alis))
			    (let* ((p (car alis))
				   (newkey (car p)))
			      (if (assq newkey present-alis)
				  (lp (cdr alis)
				      prepend)
				  (lp (cdr alis)
				      (cons p prepend))))))))
		(else
		 alis)))))
    ;; now check (todo: make namespaceetc aware!..) if attributes are
    ;; already present
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

(TEST
 > (with-sxml-element '(foo (@ (baz "ha")) "foo") vector)
 #(foo (@ (baz "ha")) ("foo"))
 )

(define unbound (gensym))

(define (sxml-element-attribute-ref element attrname #!optional (missing unbound))
  ;; attrname must be a symbol
  (with-sxml-element-attributes/else element
				     (lambda (attrs)
				       (cond ((assq attrname (cdr attrs))
					      => cdr)
					     (else #f)))
				     (lambda ()
				       (if (eq? missing unbound)
					   (error "missing sxml-attribute:" attrname)
					   missing))))

(define (sxml-attribute-ref attributes attrname)
  ;; attrname must be a symbol
  (if (eq? '@ (car attributes))
      (cond ((assq attrname (cdr attributes))
	     => cdr)
	    (else #f))
      (error "expected sxml-attributes, got:" attributes attrname)))

(define (sxml-attribute-value-ref attributes attrname)
  ;; attrname must be a symbol
  (cond ((assq attrname (cdr attributes))
	 => cadr)
	(else #f)))
;;^- hm mix max: 2 things different!.


;;XX use this in all the older functions
(define sxml:attribute-value
  (lambda (v)
    (let ((v* (cdr v)))
      (if (pair? v*)
	  (if (null? (cdr v*))
	      (car v*)
	      (error "more than one value in attributes for:" (car v)))
	  (error "missing attribute value in attributes for:" (car v))))))

(define (sxml-attributes:ref attrs namesym #!optional (missing unbound))
  (if (and (pair? attrs)
	   (eq? '@ (car attrs)))
      (cond ((assq namesym (cdr attrs))
	     => sxml:attribute-value)
	    (else
	     (if (eq? missing unbound)
		 (error "missing attribute named" namesym)
		 missing)))
      (error "expected sxml-attributes, got:" attrs namesym)))

(define sxml-attributes.ref sxml-attributes:ref)

(TEST
 > (sxml-attributes.ref '(@ (foo 1) (bar 2)) 'bar)
 2
 > (%try-error (sxml-attributes.ref '(@ (foo 1) (bar 2 3)) 'bar))
 #(error "more than one value in attributes for:" bar)
 > (%try-error (sxml-attributes.ref '(@ (foo 1) (bar . 2)) 'bar))
 #(error "missing attribute value in attributes for:" bar)
 > (%try-error (sxml-attributes.ref '(@ (foo 1) (bar . 2)) 'baz))
 #(error "missing attribute named" baz)
 > (%try-error (sxml-attributes.ref '(@ (foo 1) (bar . 2)) 'baz #f))
 #f
 > (%try-error (sxml-attributes.ref '((foo 1) (bar . 2)) 'baz #f))
 #(error "expected sxml-attributes, got:" ((foo 1) (bar . 2)) baz))

(define-if-not-defined sxml:nothing (gensym 'sxml-nothing))
;; not to be stored in SXML, just to be used in sxml library API!
;; ah, or just use (void) ok? hmm? or not?
;; Call it sxml:missing ?

(define (sxml-attributes.update attrs namesym fn)
  (if (and (pair? attrs)
	   (eq? '@ (car attrs)))
      (cons
       '@
       (let rec ((l (cdr attrs)))
	 (if (null? l)
	     ;; ok, actually add it at the end?
	     (let ((v (fn sxml:nothing)))
	       (if (eq? v sxml:nothing)
		   '()
		   (cons (list namesym v) '())))
	     (let-pair ((a l*) l)
		       (if (eq? (car a) namesym)
			   (let ((v (fn (sxml:attribute-value a))))
			     (if (eq? v sxml:nothing)
				 l*
				 (cons (list namesym v) l*)))
			   (cons a (rec l*)))))))
      (error "expected sxml-attributes, got:" attrs namesym)))

(TEST
 ;; (tests for sxml:nothing: see test forms for set and delete)
 > (sxml-attributes.update '(@ (foo 1) (bar 2) (baz 3)) 'bar inc)
 (@ (foo 1) (bar 3) (baz 3)))

(define (sxml-attributes.set attrs namesym val)
  (sxml-attributes.update attrs namesym (lambda (v) val)))

(TEST
 > (sxml-attributes.set '(@ (foo 1) (bar 2)) 'baz "baz")
 (@ (foo 1) (bar 2) (baz "baz"))
 > (sxml-attributes.set '(@ (foo 1) (bar 2)) 'bar "baz")
 (@ (foo 1) (bar "baz"))
 > (sxml-attributes.set '(@ (foo 1) (bar 2)) 'foo "baz")
 (@ (foo "baz") (bar 2)))

(define (sxml-attributes.set-missing attrs namesym val)
  (sxml-attributes.update
   attrs namesym (lambda (v)
		   (if (eq? v sxml:nothing)
		       val
		       v))))

(TEST
 > (sxml-attributes.set-missing '(@ (foo 1) (bar 2)) 'baz "baz")
 (@ (foo 1) (bar 2) (baz "baz"))
 > (sxml-attributes.set-missing '(@ (foo 1) (bar 2)) 'bar "baz")
 (@ (foo 1) (bar 2))
 > (sxml-attributes.set-missing '(@ (foo 1) (bar 2)) 'foo "baz")
 (@ (foo 1) (bar 2)))


(define (sxml-attributes.delete attrs namesym)
  (sxml-attributes.update attrs namesym (lambda (v) sxml:nothing)))

(TEST
 > (sxml-attributes.delete '(@ (foo 1) (bar 2)) 'foo)
 (@ (bar 2))
 > (sxml-attributes.delete '(@ (foo 1) (bar 2)) 'bar)
 (@ (foo 1))
 > (sxml-attributes.delete '(@ (foo 1) (bar 2) (baz 3)) 'bar)
 (@ (foo 1) (baz 3))
 > (sxml-attributes.delete '(@ (foo 1) (bar 2)) 'z)
 (@ (foo 1) (bar 2)))




(define (sxml-element-bodytext element)
  ;; merge all text elements in the body to one string. Is is an error
  ;; if an element is there.  todo: deal with lists of things, and #f,
  ;; #!void, single chars
  (apply string-append (sxml-element-body element)))


(define (sxml-element-search-subelement-with-name/attribute/value
	 element eltname attrname attrvalue #!optional (tail '()))
  (with-sxml-element
   element
   (lambda (name attrs body)
     (stream-map/filter/tail
      (lambda (v yes no)
	(with-sxml-element/else
	 v
	 (lambda (name attrs body)
	   (if (and (eq? name eltname)
		    (string=? (sxml-attribute-value-ref attrs attrname)
			      attrvalue))
	       (yes v)
	       (no)))
	 no))
      tail
      body))))

;; (todo: too close to the above)
(define (sxml-element-search-subelement-with-name element eltname)
  ;; only finds those directly inside, not recursively
  (with-sxml-element element
		     (lambda (name attrs body)
		       (stream-map/filter (lambda (v yes no)
				     (with-sxml-element/else
				      v
				      (lambda (name attrs body)
					(if (eq? name eltname)
					    (yes v)
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
  (if (null? path)
      ;; * vs . ?
      (append elements tail)
      (let ((eltname (car path))
	    (rest (cdr path)))
	(fold-right
	 (lambda (v tail)
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

;; x for exception or extract?

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

;; call this atom-whitespace? or whitespace? ?
(define (sxml-whitespace? obj)
  (and (string? obj)
       (string-all-whitespace? obj)))

(define (sxml-strip-whitespace doc)
  (with-sxml-element/else
   doc
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
  (string-count-chars//accessor
   string
   char-criterion
   (lambda (str len pos)
     (string-ref str pos))))
(define (string-count-trailing-chars string char-criterion)
  (string-count-chars//accessor
   string
   char-criterion
   (lambda (str len pos)
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

;(define shy)


(define sxml-element.body sxml-element-body)
(define sxml-element.bodytext sxml-element-bodytext)
;; or actually even dot-oo versions? are you crazy?
