;; constructors for XHTML SXML that accept DSSSL style keyword
;; arguments for attributes.

;; (Todo?: use per-element keyword checking)

(require easy test)

(both-times

 (def xhtml-element-names
      '(a
	abbr
	acronym
	address
	applet
	area
	b
	base
	basefont
	bdo
	big
	blockquote
	body
	br
	button
	caption
	center
	cite
	code
	col
	colgroup
	dd
	del
	dfn
	dir
	div
	dl
	dt
	em
	fieldset
	font
	form
	h1
	h2
	h3
	h4
	h5
	h6
	head
	hr
	html
	i
	iframe
	img
	input
	ins
	isindex
	kbd
	label
	legend
	li
	link
	map
	menu
	meta
	noframes
	noscript
	object
	ol
	optgroup
	option
	p
	param
	pre
	q
	s
	samp
	script
	select
	small
	span
	strike
	strong
	style
	sub
	sup
	table
	tbody
	td
	textarea
	tfoot
	th
	thead
	title
	tr
	tt
	u
	ul
	var
	*comment*))

 (def xhtml-attribute-names
      '(abbr
	accept
	accept-charset
	accesskey
	action
	align
	alink
	alt
	archive
	axis
	background
	bgcolor
	border
	cellpadding
	cellspacing
	char
	charoff
	charset
	checked
	cite
	class
	classid
	clear
	code
	codebase
	codetype
	color
	cols
	colspan
	compact
	content
	coords
	data
	datetime
	declare
	defer
	dir
	disabled
	enctype
	face
	for
	frame
	frameborder
	headers
	height
	href
	hreflang
	hspace
	http-equiv
	id
	ismap
	label
	lang
	language
	link
	longdesc
	marginheight
	marginwidth
	maxlength
	media
	method
	multiple
	name
	nohref
	noshade
	nowrap
	object
	onblur
	onchange
	onclick
	ondblclick
	onfocus
	onkeydown
	onkeypress
	onkeyup
	onload
	onmousedown
	onmousemove
	onmouseout
	onmouseover
	onmouseup
	onreset
	onselect
	onsubmit
	onunload
	profile
	prompt
	readonly
	rel
	rev
	rows
	rowspan
	rules
	scheme
	scope
	scrolling
	selected
	shape
	size
	span
	src
	standby
	start
	style
	summary
	tabindex
	target
	text
	title
	type
	usemap
	valign
	value
	valuetype
	vlink
	vspace
	width
	xml:lang
	xml:space
	xmlns))
 
 (def xhtml-attribute:maybe-keyword->symbol
      (let ((t (list->table
		(map (lambda (s)
		       (cons (symbol.keyword s) s))
		     xhtml-attribute-names))))
	(lambda (v)
	  (table-ref t v #f)))))

(def (make-xhtml-element/ name)
     (lambda vals
       (let lp ((vals vals)
		(atts '()))
	 (let ((construct
		(lambda ()
		  (cons name
			(if (null? atts)
			    vals
			    (cons (cons '@ atts) ;; reverse them back?
				  vals))))))
	   (if (null? vals)
	       (construct)
	       (let-pair ((v vals*) vals)
			 (if (keyword? v)
			     (cond ((xhtml-attribute:maybe-keyword->symbol v)
				    => (lambda (k)
					 (lp (cdr vals*)
					     (cons (list k (car vals*)) atts))))
				   (else
				    (error "unknown XHTML attribute name:" v)))
			     ;; stop processing keywords
			     (construct))))))))

(insert-result-of
 (cons 'begin
       (map (lambda (name)
	      `(begin
		 (define ,(symbol-append "xhtml#" name)
		   (make-xhtml-element/ ',name))
		 (define ,(symbol-append "xhtml:" name)
		   ,(symbol-append "xhtml#" name))))
	    xhtml-element-names)))


;; NOTE: does not import |xhtml#map|, since the list function is way
;; more often used than the HTML element.

(defmacro (XHTML . es)
  `(##let ()
	  (##namespace ("xhtml#" ,@(filter (lambda (n) (not (eq? n 'map)))
					   xhtml-element-names)))
	  (##define-syntax ,'unquote
			   (lambda (stx)
			     (cj-sourcify-deep
			      (match*
			       stx
			       ((_ expr)
				`(##let ()
					(##namespace (""))
					,expr)))
			      stx)))
	  ,(if (one? es)
	       (car es)
	       ;; but who says really that I want list and not
	       ;; begin. Should XHTML not be a template thing but just
	       ;; set up context thing? Or not, because making it one
	       ;; template at a time means it could optimize in the
	       ;; future.
	       `(list '##begin ,@es))))

(TEST
 > (xhtml#a href: "foo" "bar")
 (a (@ (href "foo")) "bar")
 ;; > (XHTML (map (C a href: _ "bar") '("a" "b")))
 ;; (map #<procedure #3> ("a" "b"))
 > (XHTML (,map (C a href: _ "bar") '("a" "b")))
 ((a (@ (href "a")) "bar") (a (@ (href "b")) "bar"))
 > (XHTML (b "foo"))
 (b "foo")
 > (XHTML (inc 4))
 5
 > (%try-error (xhtml#a a: 1 b: "fo"))
 #(error "unknown XHTML attribute name:" a:)
 ;; ((BTW, source location? Values are runtime.))
 > (XHTML (a `("fo" (inc 4))))
 (a ("fo" (inc 4)))
 > (XHTML (a `("fo" ,(inc 4))))
 (a ("fo" 5))
 ;; > (XHTML (a `("fo" ,(map inc '(4)))))
 ;; (a ("fo" (map #<procedure #2 inc> (4))))
 > (XHTML (a `("fo" ,(,map inc '(4)))))
 (a ("fo" (5)))
 ;; fun :)
 )

;; multiple forms
(TEST
 > (sxml->html-string-fragment (XHTML (p 1) (p 2)))
 "<p>1</p><p>2</p>"
 )
;; Hmm do I really want this? Danger because those are not full
;; documents. XX But it's the serializer's job to notice this,
;; actually.



;; hehe danger: some of my unhygienic macros might fail in this
;; context..

(def (xhtml:begin . body)
     `(##begin ,@body))

