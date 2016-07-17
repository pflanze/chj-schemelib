;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; for POSIX filesystem or HTTP paths

;; (originally) partially following
;; http://hackage.haskell.org/packages/archive/system-filepath/0.4.6/doc/html/src/Filesystem-Path.html

(require easy ;; incl. more-oo
	 test
	 string-util-1
	 (cj-functional-2 =>))

(def (posixpath-segment? v)
     (and (nonempty-string? v)
	  (not (string-contains? v "/"))))

(def (collapsed-posixpath-segment? v)
     (and (nonempty-string? v)
	  (not (or (string.dot? v)
		   (string-contains? v "/")))))

(def list-of-string?             (list-of string?))
(def list-of-nonempty-string?    (list-of nonempty-string?))
(def list-of-posixpath-segment?  (list-of posixpath-segment?))
(def list-of-collapsed-posixpath-segment?
     (list-of collapsed-posixpath-segment?))


(def. (string.dot? v)
  (string=? v "."))

(def. (string.dotdot? v)
  (string=? v ".."))

(defenum posixpath-type
  directory file)


(class segmentedpath
       (subclass posixpath
		 (subclass uncollapsed-posixpath
			   (struct 
			    #(boolean? absolute?)
			    #(list-of-posixpath-segment? segments)
			    #!optional
			    #((maybe posixpath-type?) maybe-type)))

		 (subclass collapsed-posixpath
			   (struct 
			    #(boolean? absolute?)
			    #(list-of-collapsed-posixpath-segment? segments)
			    #!optional
			    #((maybe posixpath-type?) maybe-type)))))

(def. (posixpath.null? p)
  (null? (.segments p)))

(def. (posixpath.directory? p #!optional (unknown false/0))
  (case (.maybe-type p)
    ((directory) #t)
    ((file) #f)
    (else (unknown))))

(def. (posixpath.file? p #!optional (unknown false/0))
  (case (.maybe-type p)
    ((file) #t)
    ((directory) #f)
    (else (unknown))))


;; (do *not* expect list-of-posixpath-segment, as it is meant to be
;; used after string-split)--ah, but "." is still ok for that?
(def. list-of-string.posixpath
  (lambda (l #!optional #((maybe posixpath-type?) type))
    (if (null? l)
	(error "what's this?")
	;; ^ rootpath or ? ah doesn't happen from string-split,
	;; splitting "" gives ("") yeah, remember, stupid right?
	(let ((absolute? (string-empty? (car l)))
	      (looks-like-directory? ((either string-empty?
					      .dot?
					      .dotdot?) (last l)))
	      (ss (filter (complement string-empty?) l)))
	  (uncollapsed-posixpath
	   absolute?
	   ss
	   (or type
	       ;; XX instead give error when in conflict with each other?
	       (and looks-like-directory? 'directory)))))))

(def. (string.posixpath s #!optional type)
  (if (string-empty? s)
      (error "not accepting empty string as a posixpath")
      (list-of-string.posixpath (string-split s #\/) type)))

(TEST
 > (%try-error (.posixpath ""))
 #(error "not accepting empty string as a posixpath")
 > (.posixpath "/foo")
 #((uncollapsed-posixpath) #t ("foo") #f)
 > (.posixpath "/foo/")
 #((uncollapsed-posixpath) #t ("foo") directory)
 > (.posixpath "/")
 #((uncollapsed-posixpath) #t () directory)
 > (.string (.posixpath "/"))
 "/"
 > (.posixpath "./foo")
 #((uncollapsed-posixpath) #f ("." "foo") #f)
 > (.posixpath "foo")
 #((uncollapsed-posixpath) #f ("foo") #f)
 > (.string (.posixpath '("foo" "bar")))
 "foo/bar"
 > (%try-error (.string (.posixpath '("foo/f" "bar"))))
 #(error "segments does not match list-of-posixpath-segment?:" ("foo/f" "bar"))
 )

(def. (posixpath.string p)
  (let* ((ss (.segments p))
	 (s (strings-join ss "/")))
    (if (null? ss)
	(if (.absolute? p)
	    "/"
	    "./")
	(string-append
	 (if (.absolute? p)
	     ;; (note that prepending an "" segment before strings-join
	     ;; would not work in the case of the root dir)
	     "/"
	     "")
	 s
	 (if (and (pair? ss) ;; <- superfluous
		  (.directory? p))
	     "/"
	     "")))))

(TEST
 > (.string (.posixpath "/foo"))
 "/foo"
 > (.string (.posixpath "/foo//bar"))
 "/foo/bar"
 > (.string (.posixpath "foo//bar"))
 "foo/bar"
 > (.string (.posixpath "//foo//bar"))
 "/foo/bar"
 )

(def. (posixpath.append a b)
  (if (.absolute? b)
      b
      (if (.file? a)
	  (error "first path is to a file:" (.string a))
	  (if (.null? b)
	      (.maybe-type-set a 'directory)
	      ;;^ XX btw assert that b was one? i.e. that a null path is
	      ;;directory if type is given
	      ((if (and (collapsed-posixpath? a)
			(collapsed-posixpath? b)
			(not (.dotdot? (car (.segments b)))))
		   collapsed-posixpath
		   uncollapsed-posixpath)
	       (.absolute? a)
	       (append (.segments a) (.segments b))
	       (.maybe-type b))))))

(TEST
 > (.string (.append (.posixpath "//foo//bar") (.posixpath "baz")))
 "/foo/bar/baz"
 > (.string (.append (.posixpath "//foo//bar") (.posixpath "../baz")))
 "/foo/bar/../baz"
 > (.string (.append (.posixpath "//foo//bar") (.posixpath "/baz")))
 "/baz"
 > (.string (.append (.posixpath "//foo//bar") (.posixpath "/baz/")))
 "/baz/"
 > (.string (.append (.posixpath "//foo//bar") (.posixpath "baz/")))
 "/foo/bar/baz/"
 > (.append (.posixpath "//foo//bar") (.collapse (.posixpath "../baz/")))
 #((uncollapsed-posixpath) #t ("foo" "bar" ".." "baz") directory)
 > (.append (.collapse (.posixpath "//foo//bar"))
	    (.collapse (.posixpath "../baz/")))
 #((uncollapsed-posixpath) #t ("foo" "bar" ".." "baz") directory)
 > (.append (.collapse (.posixpath "//foo//bar"))
	    (.collapse (.posixpath "baz/")))
 #((collapsed-posixpath) #t ("foo" "bar" "baz") directory)
 )


;;; collapse (or 'simplify', 'canonicalize', 'normalize'?) -----------------

;; Note that if any of the elements are symbolic links, collapse may
;; change which file the path resolves to.

;; resolve just "..", assume there are no "."
(def. (list-of-posixpath-segment.normalize l #!optional (values values))
  (let lp ((l l)
	   (res '())
	   (levels-above 0))
    (if (null? l)
	(values (reverse res) levels-above)
	(let-pair ((a l*) l)
		  (if (.dotdot? a)
		      (if (null? res)
			  (lp l* res (inc levels-above))
			  (lp l* (cdr res) levels-above))
		      (lp l* (cons a res) levels-above))))))

(TEST
 > (.normalize '("foo" "bar") vector)
 #(("foo" "bar") 0)
 > (.normalize '("foo" "..") vector)
 #(() 0)
 > (.normalize '(".." "foo") vector)
 #(("foo") 1)
 > (.normalize '(".." "foo" ".." ".." "baz" "hm") vector)
 #(("baz" "hm") 2)
 > (.normalize '(".." "foo" ".." ".." "baz" ".." "..") vector)
 #(() 3) ;; fun that this simple idea actually works? I doubt it yet tho
 )


(def. (list-of-posixpath-segment.collapse l values)
  (list-of-posixpath-segment.normalize
   (filter (complement string.dot?) l)
   values))

(def. (uncollapsed-posixpath.collapse p)
  (let-uncollapsed-posixpath
   ((absolute? segments maybe-type) p)

   (collapsed-posixpath absolute?
			(list-of-posixpath-segment.collapse
			 segments
			 (lambda (l* levels-above)
			   (if (or (not absolute?)
				   (zero? levels-above))
			       (make-list/tail levels-above ".." l*)
			       (error "absolute path pointing outside the root:"
				      p))))
			maybe-type)))

(def. (collapsed-posixpath.collapse p) p)

(def posixpath-dot (.collapse (.posixpath ".")))
(def posixpath-dotdot (.posixpath ".."))

(TEST
 > (.string (.collapse (.posixpath "/foo/bar/../baz")))
 "/foo/baz"
 > (.string (.collapse (.posixpath "/foo/bar/../../baz")))
 "/baz"
 > (%try-error (.string (.collapse (.posixpath "/foo/bar/../../../baz"))))
 #(error
   "absolute path pointing outside the root:"
   #((uncollapsed-posixpath) #t ("foo" "bar" ".." ".." ".." "baz") #f))
 ;; would get "/../baz" without the exception
 > (.string (.collapse (.posixpath "bar/./../baz/./..")))
 "./"
 > (.string (.collapse (.posixpath "/foo/..")))
 "/"
 > (.string (.collapse (.posixpath "foo/..")))
 "./"
 > (.collapse (.posixpath "./foo/../."))
 #((collapsed-posixpath) #f () directory)
 > (.string (.collapse (.posixpath "./foo/../.")))
 "./"
 > (.string (.collapse (.posixpath "./foo/.././..")))
 "../"
 )



;;; filename, parent --------------------------------------------------

;; hmm, not call those basename and dirname sigh?
;; (well, not calling them that makes sense on non-collapsed paths; but here?)


;; (def. collapsed-posixpath.filename
;;   (compose last .segments))

(def. (collapsed-posixpath.if-filename p then els)
  (let ((ss (.segments p)))
    (if (null? ss)
	(els)
	(last ss))))

(def. (collapsed-posixpath.xfilename p)
  (collapsed-posixpath.if-filename
   p
   identity
   (cut error "can't take filename of empty path")))

(def. (collapsed-posixpath.maybe-filename p)
  (collapsed-posixpath.if-filename
   p
   identity
   false/0))


(TEST
 > (.maybe-filename (.collapse (.posixpath "//foo//bar")))
 "bar"
 > (.maybe-filename (.collapse (.posixpath "bar")))
 "bar"
 > (%try-error (.xfilename (.collapse (.posixpath "/"))))
 #(error "can't take filename of empty path")
 > (.maybe-filename (.collapse (.posixpath ".")))
 #f
 > (.xfilename (.collapse (.posixpath "foo/bar/..")))
 "foo"
 )


;; really collapsed-list-of-posixpath-segment.parent but that's not
;; easy to check.. would have to be represented in the type
;; explicitely.

;; (def list-of-posixpath-segment.parent
;;   (compose* reverse cdr reverse))

(def (list-of-posixpath-segment.parent l)
     (let ((r (reverse l)))
       (reverse (if (.dotdot? (car r))
		    (cons ".." r)
		    (cdr r)))))

(def. (collapsed-posixpath.if-parent p then els)
  (let-collapsed-posixpath
   ((absolute? segments maybe-type) p)

   (if (null? segments)
       (if absolute?
	   (els)
	   posixpath-dotdot)
       (collapsed-posixpath absolute?
			    (list-of-posixpath-segment.parent segments)
			    'directory))))

(def. (collapsed-posixpath.xparent p)
  (collapsed-posixpath.if-parent
   p
   identity
   (cut error "can't take parent of root dir")))

(def. (collapsed-posixpath.maybe-parent p)
  (collapsed-posixpath.if-parent
   p
   identity
   false/0))

(TEST
 > (.string (.xparent (.collapse (.posixpath "//foo//bar"))))
 "/foo/"
 > (.string (.xparent (.collapse (.posixpath "//foo"))))
 "/"
 > (.string (.xparent (.collapse (.posixpath "foo"))))
 "./"
 > (.string (.xparent (.collapse (.posixpath "."))))
 "../"
 > (.string (.xparent (.collapse (.posixpath ".."))))
 "../../"
 )


;;; add --------------------------------------------------

;; well we have append already; but, for collapsed paths, have one
;; that re-collapses would be nice, right?

;; well, could do 'cheaply' for now:

;; actually no need to require them to be collapsed, here..

(def. posixpath.add
  (lambda (a #(posixpath? b))
    (.collapse (.append a b))))

(TEST
 > (.string (.add (.posixpath "/foo/baz") (.posixpath "../bar.html")))
 "/foo/bar.html"
 > (%try-error (.add (.posixpath "/foo/baz.html" 'file)
		     (.posixpath "../bar.html")))
 #(error "first path is to a file:" "/foo/baz.html")
 )


;; a is a chroot that b cannot leave; b is expected to be absolute
(def. (posixpath.chroot-add a #(posixpath? b))
  (let ((b* (if (collapsed-posixpath? b)
	       b
	       (.collapse b))))
    (if (.absolute? b*)
	(posixpath.append a (.absolute?-set b* #f))
	(error ".chroot-add: path b is not absolute:" b))))

(TEST
 > (def t* (on .posixpath .chroot-add))
 > (def t (compose .string t*))
 > (%try-error (t "/foo/baz" "../bar.html"))
 #(error
   ".chroot-add: path b is not absolute:"
   #((uncollapsed-posixpath) #f (".." "bar.html") #f))
 > (t "/foo/baz" "/bar.html")
 "/foo/baz/bar.html"
 > (t "/foo/baz/" "/bar.html")
 "/foo/baz/bar.html"
 > (t "/foo/baz/" "/./bar.html")
 "/foo/baz/bar.html"
 > (t "/foo/baz/" "/f/../bar.html")
 "/foo/baz/bar.html"
 > (%try-error (t "/foo/baz/" "/../bar.html"))
 #(error
   "absolute path pointing outside the root:"
   #((uncollapsed-posixpath) #t (".." "bar.html") #f))
 > (t "/foo/.." "/bar.html")
 ;; .chroot-add does not collapse a
 "/foo/../bar.html"

 ;; relative a is ok:
 > (t "foo/.." "/bar.html")
 "foo/../bar.html"
 > (t ".." "/bar.html")
 "../bar.html"
 > (t "../." "/bar.html")
 ".././bar.html"
 ;; and of course, very usual case?:
 > (t "." "/bar.html")
 "./bar.html"
 > (t "." "/.")
 "./"
 ;; correct directory vs. file handling:
 > (t* ".." "/.")
 #((uncollapsed-posixpath) #f ("..") directory)
 > (t* ".." "/")
 #((uncollapsed-posixpath) #f ("..") directory)
 > (t* ".." "/foo")
 #((uncollapsed-posixpath) #f (".." "foo") #f)
 > (%try-error (.chroot-add (.posixpath "foo") (.posixpath "/bar" 'file)))
 #((uncollapsed-posixpath) #f ("foo" "bar") file)
 ;; superfluous since this check in .append is already tested, but...:
 > (%try-error (.chroot-add (.posixpath "foo" 'file) (.posixpath "/bar")))
 #(error "first path is to a file:" "foo")
 ;; hm BTW interesting, a path ending in a slash can be a file?
 > (.chroot-add (.posixpath "foo") (.posixpath "/bar/" 'file))
 #((uncollapsed-posixpath) #f ("foo" "bar") file)
 ;; check collapse 'flag' maintenance
 > (.chroot-add (.collapse (.posixpath "foo")) (.posixpath "/bar/" 'file))
 #((collapsed-posixpath) #f ("foo" "bar") file))


;; Idea: a type wrapper that makes .add do .chroot-add, so that users
;; can enforce "safety by data (configuration)" instead (or in
;; addition to) "safety by program". ok?

(defstruct chroot-path
  value)
;;(heh .value is used a lot; well we want to optimize at one point..)

(def. (chroot-path.chroot-add a b)
  (let-chroot-path ((v) a)
		   (.chroot-add v b)))

;; and now the point of it:
(def. chroot-path.add chroot-path.chroot-add)

(TEST
 > (.chroot-add (chroot-path (.posixpath "foo")) (.posixpath "/bar/"))
 #((uncollapsed-posixpath) #f ("foo" "bar") directory)
 > (.add (chroot-path (.posixpath "foo")) (.posixpath "/bar/"))
 #((uncollapsed-posixpath) #f ("foo" "bar") directory)
 ;; XX BUT does it make sense?: b would probably be relative in apps
 ;; in those cases, which would make chroot-path's just give
 ;; exceptions all the time:
 > (.add (.posixpath "foo") (.posixpath "/bar/"))
 #((collapsed-posixpath) #t ("bar") directory)

 > (%try-error (.add (chroot-path (.posixpath "foo")) (.posixpath "../")))
 #(error
   ".chroot-add: path b is not absolute:"
   #((uncollapsed-posixpath) #f ("..") directory))
 ;; versus:
 > (%try-error (.add (.posixpath "foo") (.posixpath "../")))
 #((collapsed-posixpath) #f () directory))

;; /idea


;;; diff --------------------------------------------------

(def (common-prefix-drop a b)
     (let ((n (lists-common-prefix-length (list a b) string=?)))
       (values (drop a n)
	       (drop b n))))

(TEST
 > (.vector (common-prefix-drop '("a" "b")'("a" "c" "d")))
 #(("b")
   ("c" "d"))
 )

(def (cj-posixpath:ppdiff from to)
     (letv ((froms tos) ((on .segments common-prefix-drop) from to))
	   (if (and (pair? froms)
		    (.dotdot? (car froms)))
	       (error "relative from path is further up than to:"
		      (.string from)
		      (.string to))
	       ;; segments is all we need to change? Even stays
	       ;; collapsed?
	       (=> to
		   (.segments-set (make-list/tail (length froms) ".." tos))
		   (.absolute?-set #f)))))

(def (cj-posixpath:_diff from to)
     (if (.absolute? from)
	 (if (.absolute? to)
	     (cj-posixpath:ppdiff from to)
	     (error "from is absolute, to relative"))
	 (if (.absolute? to)
	     to ;; (error "from is relative, to is absolute")
	     ;; (error "diff of relative paths not yet implemented")
	     (cj-posixpath:ppdiff from to))))

(def. (collapsed-posixpath.diff from #(collapsed-posixpath? to))
  (cj-posixpath:_diff (if (.file? from)
			  (.xparent from)
			  from)
		      to))


(TEST
 > (define (t-diff a b)
     (.string (.diff (.collapse (if (posixpath? a) a (.posixpath a)))
		     (.collapse (if (posixpath? b) b (.posixpath b))))))
 > (t-diff "../baz/" "../bar.html")
 "../bar.html"
 > (t-diff (.posixpath "../baz.html" 'file) "../bar.html")
 "bar.html"
 ;; absolute paths:
 > (t-diff "/foo" "/foo/bar")
 "bar"
 > (t-diff (.posixpath "/foo" file) "/foo/bar")
 "foo/bar"
 > (t-diff "/foo/" "/foo/bar")
 "bar"
 ;; (XX: really run all tests for both absolute and
 ;; relative-from-some-imaginary root? Do more test generation?)
 
 > (def (pref v)
	(.add (.collapse (.posixpath "a/b/c"))
	      (.collapse (.posixpath v))))
 > (def (nopref v)
	(.collapse (.posixpath v)))

 > (.string (pref "../../foo"))
 "a/foo"
 > (.string (pref "../bar.html"))
 "a/b/bar.html"
 > (.string (.diff (pref "../../foo")
		   (pref "../bar.html")))
 "../b/bar.html"
 
 > (%try-error (.diff (nopref "../../foo")
		      (nopref "../bar.html")))
 #(error "relative from path is further up than to:" "../../foo" "../bar.html")

 ;; a case where relative works
 > (.string (.diff (pref "../foo")
		   (pref "../../bar.html")))
 "../../bar.html"
 > (.string (.diff (nopref "../foo")
		   (nopref "../../bar.html")))
 "../../bar.html"

 > (t-diff "../." "../bar.html")
 "bar.html"
 > (t-diff "foo/bar/" "../baz.html")
 "../../../baz.html"

 ;; (unimportant or misplaced tests:
 > (t-diff "foo/bar" "../baz.html")
 ;; yeah, currently "foo/bar" is still handled like a dir
 "../../../baz.html"
 > (t-diff "foo/bar" "bim/baz.html")
 "../../bim/baz.html"
 > (t-diff "." "bim/baz.html")
 "bim/baz.html"
 ;; )
 )


;;; Web URLs: web-add, web-diff ------------------------------------------

;; XX should this be named .url-add ? But the data types are not full
;; URLs. Should it be named .urlpath-add ?

;; ATTENTION: both base and url are meant to be untrusted data. Don't
;; give a filesystem location as base. Instead, feed the result of
;; .web-add to .chroot-add !

;; Adding of relative URLs treats the source location as a directory
;; that can directly be taken as the base if it ends in a slash,
;; otherwise the parent is used.
(def. (collapsed-posixpath.web-add base #(posixpath? url))
  (posixpath.add (if (.directory? base)
		     base
		     (.xparent base))
		 url))

(TEST
 > (define (t-add a b)
     (.string (.web-add (.collapse (.posixpath a))
			(.posixpath b))))
 > (t-add "/" "foo.png")
 "/foo.png"
 > (t-add "." "foo.png")
 ;; XXX allow this? Make (.absolute? a) a requirement?
 "foo.png"
 > (t-add "/" "foo.png")
 "/foo.png"
 ;; > (%try-error (t "/.." "foo.png"))
 ;; #(error
 ;;   "absolute path pointing outside the root:"
 ;;   #((uncollapsed-posixpath) #t (".." "foo.png") #f))
 ;; #(error
 ;;   "absolute path pointing outside the root:"
 ;;   #((uncollapsed-posixpath) #t ("..") directory))
 > (t-add "/bar" "foo.png")
 "/foo.png"
 > (%try-error (t-add "/bar" "../foo.png"))
 ;; XX should probably use an overridable (continuable) error
 #(error
   "absolute path pointing outside the root:"
   #((uncollapsed-posixpath) #t (".." "foo.png") #f))
 > (t-add "/bar/" "../foo.png")
 "/foo.png"
 > (t-add "/bar/baz" "../foo.png")
 "/foo.png"
 > (t-add "/bar/baz" ".")
 "/bar/" ;; heh good
 )

(def. (collapsed-posixpath.web-diff base #(collapsed-posixpath? url))
  (cj-posixpath:_diff (if (.directory? base)
			  base
			  (.xparent base))
		      url))

(TEST
 > (define (t a b)
     (.string (.web-diff (.collapse (.posixpath a))
			 (.collapse (.posixpath b)))))
 ;; I'm at /foo, want to go to /foo/bar
 > (t "/foo" "/foo/bar")
 "foo/bar"
 > (t "/foo/" "/foo/bar")
 "bar"
 > (t "foo" "foo/bar")
 "foo/bar"
 > (t "foo/" "foo/bar")
 "bar"
 > (t "foo/bar" "foo/bar/baz")
 "bar/baz"
 > (t "foo/bar" "../baz")
 "../../baz" ;; XXX should this give an exception?, unlike .diff ?
 )

