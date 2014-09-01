;;; Copyright 2013 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; for POSIX filesystem or HTTP paths

;; partially following
;; http://hackage.haskell.org/packages/archive/system-filepath/0.4.6/doc/html/src/Filesystem-Path.html

(require easy string-util-1 more-oo)

(def nonempty-string?
     (both string?
	   (complement string-empty?)))

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

(defenum posixpath-type
  directory file)

(def. (string.dot? v)
  (string=? v "."))

(def. (string.dotdot? v)
  (string=? v ".."))


(defstruct posixpath
  #(boolean? absolute?)
  #(list-of-posixpath-segment? segments)
  #!optional
  #((maybe posixpath-type?) maybe-type)
  #(boolean? collapsed?))

(def collapsed-posixpath?
     (both posixpath?
	   posixpath.collapsed?))


(def. (posixpath.null? p)
  (null? (.segments p)))

(def. (posixpath.directory? p #!optional (unknown false/0))
  (case (posixpath.maybe-type p)
    ((directory) #t)
    ((file) #f)
    (else (unknown))))

(def. (posixpath.file? p #!optional (unknown false/0))
  (case (posixpath.maybe-type p)
    ((file) #t)
    ((directory) #f)
    (else (unknown))))


;; (do *not* expect list-of-posixpath-segment, as it is meant to be
;; used after string-split)--ah, but "." is still ok for that?
(def. list-of-string.posixpath
  (typed-lambda
   (l #!optional #((maybe posixpath-type?) type))
   (if (null? l)
       (error "what's this?")
       ;; ^ XX rootpath or ? ah doesn't happen from string-split,
       ;; splitting "" gives ("") yeah, remember, stupid right?
       (let ((absolute? (string-empty? (car l)))
	     (looks-like-directory?
	      ((either string-empty?
		       .dot?
		       .dotdot?) (last l)))
	     (ss (filter (complement string-empty?) l)))
	 (posixpath absolute?
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
 #(posixpath #t ("foo") #f #f)
 > (.posixpath "/foo/")
 #(posixpath #t ("foo") directory #f)
 > (.posixpath "/")
 #(posixpath #t () directory #f)
 > (.string (.posixpath "/"))
 "/"
 > (.posixpath "./foo")
 #(posixpath #f ("." "foo") #f #f)
 > (.posixpath "foo")
 #(posixpath #f ("foo") #f #f)
 > (.string (.posixpath '("foo" "bar")))
 "foo/bar"
 > (%try-error (.string (.posixpath '("foo/f" "bar"))))
 #(error "segments does not match list-of-posixpath-segment?:" ("foo/f" "bar"))
 )

(def. (posixpath.string p)
  (let* ((ss (posixpath.segments p))
	 (s (strings-join ss "/")))
    (if (null? ss)
	(if (posixpath.absolute? p)
	    "/"
	    "./")
	(string-append
	 (if (posixpath.absolute? p)
	     ;; (note that prepending an "" segment before strings-join
	     ;; would not work in the case of the root dir)
	     "/"
	     "")
	 s
	 (if (and (pair? ss) ;; <- superfluous
		  (posixpath.directory? p))
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
  (if (posixpath.absolute? b)
      b
      (if (.file? a)
	  (error "first path is to a file:" (.string a))
	  (if (.null? b)
	      (.maybe-type-set a 'directory)
	      ;;^ XX btw assert that b was one? i.e. that a null path is
	      ;;directory if type is given
	      (posixpath (.absolute? a)
			 (append (.segments a) (.segments b))
			 (.maybe-type b)
			 ;;((on posixpath.collapsed? and) a b)
			 (and (posixpath.collapsed? a)
			      (posixpath.collapsed? b)
			      (not (.dotdot? (car (.segments b))))))))))

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
 #(posixpath #t ("foo" "bar" ".." "baz") directory #f)
 > (.append (.collapse (.posixpath "//foo//bar"))
	    (.collapse (.posixpath "../baz/")))
 #(posixpath #t ("foo" "bar" ".." "baz") directory #f)
 > (.append (.collapse (.posixpath "//foo//bar"))
	    (.collapse (.posixpath "baz/")))
 #(posixpath #t ("foo" "bar" "baz") directory #t)
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

(def. (posixpath.collapse p)
  (.collapsed?-set
   (.segments-update
    p
    (lambda (l)
      (list-of-posixpath-segment.collapse
       l
       (lambda (l* levels-above)
	 (make-list/tail levels-above ".." l*)))))
   #t))

(def posixpath-dot (.collapse (.posixpath ".")))
(def posixpath-dotdot (.posixpath ".."))

(TEST
 > (.string (.collapse (.posixpath "/foo/bar/../baz")))
 "/foo/baz"
 > (.string (.collapse (.posixpath "/foo/bar/../../baz")))
 "/baz"
 > (.string (.collapse (.posixpath "/foo/bar/../../../baz")))
 "/../baz" ;; XX give error instead or what?
 > (.string (.collapse (.posixpath "bar/./../baz/./..")))
 "./"
 > (.string (.collapse (.posixpath "/foo/..")))
 "/"
 > (.string (.collapse (.posixpath "foo/..")))
 "./"
 > (.collapse (.posixpath "./foo/../."))
 #(posixpath #f () directory #t)
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
  (let-posixpath
   ((absolute? segments maybe-type collapsed?) p)

   (if (null? segments)
       (if absolute?
	   (els)
	   posixpath-dotdot)
       (posixpath absolute?
		  (list-of-posixpath-segment.parent segments)
		  'directory
		  collapsed?))))

;; copypaste~
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
  (typed-lambda
   (a
    #(posixpath? b))
   (.collapse (.append a b))))

(TEST
 > (.string (.add (.posixpath "/foo/baz") (.posixpath "../bar.html")))
 "/foo/bar.html"
 > (%try-error (.add (.posixpath "/foo/baz.html" 'file)
		     (.posixpath "../bar.html")))
 #(error "first path is to a file:" "/foo/baz.html")
 )

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
	       (.segments-set to
			      (make-list/tail (length froms) ".." tos)))))

(def. collapsed-posixpath.diff
  (typed-lambda
   (from
    #((all-of collapsed-posixpath?
	      ;; hu was wolltichhier ?both ?
	      ) to))
   (if (.file? from)
       ;; really transparently do this?:
       (.diff (.xparent from) to)
       (if (.absolute? from)
	   (if (.absolute? to)
	       (cj-posixpath:ppdiff from to)
	       (error "from is absolute, to relative"))
	   (if (.absolute? to)
	       to ;; (error "from is relative, to is absolute")
	       ;; (error "diff of relative paths not yet implemented")
	       (cj-posixpath:ppdiff from to))))))

(TEST
 >  (.string (.diff (.collapse (.posixpath "../baz/"))
		    (.collapse (.posixpath "../bar.html"))))
 "../bar.html"
 > (.string (.diff (.collapse (.posixpath "../baz.html" 'file))
		   (.collapse (.posixpath "../bar.html"))))
 "bar.html"
 > (def (pref v) (.add (.collapse (.posixpath "a/b/c"))  v))
 > (.string (pref (.collapse (.posixpath "../../foo"))))
 "a/foo"
 > (.string (pref (.collapse (.posixpath "../bar.html"))))
 "a/b/bar.html"
 > (.string (.diff (pref (.collapse (.posixpath "../../foo")))
		   (pref (.collapse (.posixpath "../bar.html")))))
 "../b/bar.html"
 > (def pref identity)
 > (%try-error (.diff (pref (.collapse (.posixpath "../../foo")))
		      (pref (.collapse (.posixpath "../bar.html")))))
 #(error "relative from path is further up than to:" "../../foo" "../bar.html")

 ;; a case where relative works
 > (def (pref v) (.add (.collapse (.posixpath "a/b/c"))  v))
 > (.string (.diff (pref (.collapse (.posixpath "../foo")))
		   (pref (.collapse (.posixpath "../../bar.html")))))
 "../../bar.html"
 > (def pref identity)
 > (.string (.diff (pref (.collapse (.posixpath "../foo")))
		   (pref (.collapse (.posixpath "../../bar.html")))))
 "../../bar.html"

 > (.string (.diff (.collapse (.posixpath "../."))
		   (.collapse (.posixpath "../bar.html"))))
 "bar.html"
 > (.string (.diff (.collapse (.posixpath "foo/bar/"))
		   (.collapse (.posixpath "../baz.html"))))
 "../../../baz.html"
 ;; (unimportant or misplaced tests:
 > (.string (.diff (.collapse (.posixpath "foo/bar"))
		   (.collapse (.posixpath "../baz.html"))))
 ;; yeah, currently "foo/bar" is still handled like a dir
 "../../../baz.html"
 > (.string (.diff (.collapse (.posixpath "foo/bar"))
		   (.collapse (.posixpath "bim/baz.html"))))
 "../../bim/baz.html"
 > (.string (.diff (.collapse (.posixpath "."))
		   (.collapse (.posixpath "bim/baz.html"))))
 "bim/baz.html"
 ;; )
 )


;; XX Hmm create a .httppath that defaults to file semantics?
;; "foo/bar" would be a 'file'. ? Or add more types there?

