;;; Copyright 2017-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; constructors for XHTML SXML that accept DSSSL style keyword
;; arguments for attributes.

;; (Todo?: use per-element keyword checking)

(require easy test)

(include "cj-standarddeclares.scm")


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
	xmlns
	;; and, XX bad HACK, HTML 5 attributes:
	integrity
	crossorigin))
 
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

;; construct (export ) form; didn't expect this to be valid, but hey,
;; why not (and actually maybe a sensible way to go about it in
;; general: expand the form itself in its module's context, too?)
(insert-result-of
 (cons 'export
       (append '((macro XHTML)
		 (macro HTML)
		 xhtml:begin
		 xhtml:begin*)
	       xhtml-element-names)))

(insert-result-of
 (cons 'begin
       (map (lambda (name)
	      `(begin
		 (define ,(symbol-append "xhtml#" name)
		   (make-xhtml-element/ ',name))
		 (define ,(symbol-append "xhtml:" name)
		   ,(symbol-append "xhtml#" name))))
	    xhtml-element-names)))


;; Do not import some symbols that are commonly used for other
;; things. You can still access |xhtml#map| or |xhtml#s| fully
;; qualified.

(def (xhtml:no-import? v)
     (case v
       ((map s) #t)
       (else #f)))

(def (XHTML-expand es)
     `(##let ()
	     (##namespace ("xhtml#" ,@(filter (complement xhtml:no-import?)
					      xhtml-element-names)))
	     (##define-syntax
	      ,'unquote
	      (lambda (stx)
		(cj-sourcify-deep
		 (match*
		  stx
		  ((_ expr)
		   `(##let ()
			   (##namespace (""))
			   ;; report on invalid syntax, OK?
			   (##define-syntax
			    ,'unquote
			    (lambda (stx)
			      (error "invalid use of unquote")))
			   ,expr)))
		 stx)))
	     ,(if (one-item? es)
		  (car es)
		  ;; but who says really that I want list and not
		  ;; begin. Should XHTML not be a template thing but just
		  ;; set up context thing? Or not, because making it one
		  ;; template at a time means it could optimize in the
		  ;; future.
		  `(list '##begin ,@es))))

(defmacro (XHTML . es)
  (XHTML-expand es))

(defmacro (HTML . es)
  (XHTML-expand es))

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
 > (XHTML (a `("fo" ,(,map inc-function '(4)))))
 (a ("fo" (5)))
 ;; fun :)
 )

(TEST
 ;; proper handling of unquote
 > (def a 100)
 > (XHTML ,a)
 100
 > (eq? (XHTML a) xhtml:a)
 #t
 > (with-exception-catcher error-exception-message (& (eval '(XHTML ,,a))))
 "invalid use of unquote")


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

;; Safer:
(def (xhtml:begin* body)
     (cons `##begin body))
