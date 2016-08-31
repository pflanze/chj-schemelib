;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test
	 (tree-util flatten)
	 (cj-io-util read-lines xcall-with-input-process)
	 tsort
	 Status)


(TEST
 > (simple-basename ".")
 "." ;; ok?
 > (simple-basename "./bar/foo.scm")
 "foo.scm"
 > (scm-basename "./bar/foo.scm")
 "foo"
 > (scm-basename "bar.scm")
 "bar"
 > (%try-error (scm-basename "./bar/foo"))
 #(error "not a path with suffix '.scm':" "./bar/foo")
 > (%try-error (scm-basename "./bar/foo.scmx"))
 #(error "not a path with suffix '.scm':" "./bar/foo.scmx"))

(TEST
 > (%try-error (path-string.modulename "/foo"))
 #(error "not a path with suffix '.scm':" "/foo")
 > (%try-error (path-string.modulename "/foo.scm"))
 #(error "need relative path, got:" "/foo.scm")
 > (%try-error (base-string.modulename "/foo"))
 #(error "need relative base string, got:" "/foo")
 > (base-string.modulename "bar")
 bar
 > (base-string.modulename "lib/bar")
 bar
 > (path-string.modulename "bar.scm")
 bar
 > (path-string.modulename "lib/bar.scm")
 bar
 > (path-string.modulename "lib/foo/bar.scm")
 foo/bar)



(def (path-string.topo-relation p)
     (path-string.relation p topo-relation))

(TEST
 ;; well those are evil of course, will break upon module changes
 > (path-string.topo-relation "lib/require-util.scm")
 #((topo-relation)
   require-util
   (easy test tree-util cj-io-util tsort Status))
 ;; > (path-string.topo-relation "tsort.scm")
 ;; #((topo-relation) tsort (easy test alist))
 )

(def (modulepaths-tsort paths)
     (let* ((rs (map path-string.topo-relation paths))
	    (z (map cons (map .name rs) paths)))
       (map (C symbol-alist-ref z _) (topo.sort* rs))))


(def (modulepaths-in-dir dir #!optional (tail '()) normalize?)
     (parameterize
      ((current-directory dir))
      (map/tail (if normalize? path-normalize identity)
		tail
		(xcall-with-input-process
		 (list path: "gls" arguments: '("*.scm"))
      
		 read-lines))))


(def (modulepaths-in-dirs dirpaths #!optional (tail '()))
     ;;(flatten (map modulepaths-in-dir dirpaths))
     (fold-right modulepaths-in-dir tail dirpaths))

(def (lib)
     (modulepaths-tsort (modulepaths-in-dir "lib")))
(def (mydb)
     (modulepaths-tsort (modulepaths-in-dirs '("lib" "mydb"))))


;; a single-dependency representation
(class requires
       (struct name dependency))

(def (modulepaths-satisfying? paths) -> Status?
     (call/cc
      (lambda (report)
	(let* ((all-rs (map path-string.topo-relation paths)))
	  ;; nw how. wat's already loaded  walk through.
	  (let lp ((rs all-rs)
		   (loaded '()))
	    (if (null? rs)
		(Success)
		(let-pair
		 ((r rs*) rs)
		 (for-each
		  (lambda (d)
		    (unless (memq d loaded)
			    (report (Failure
				     (requires (.name r) d)))))
		  (.deps r))
		 (lp rs*
		     (cons (.name r) loaded)))))))))

(TEST
 > (modulepaths-satisfying? '("lib/Maybe.scm" "lib/easy.scm"))
 #((Failure) #((requires) Maybe easy))
 > (modulepaths-satisfying? '("lib/easy-1.scm" "lib/Maybe.scm"))
 #((Failure) #((requires) easy-1 define-macro-star))
 > (modulepaths-satisfying? '("lib/cj-source.scm"
			      "lib/define-macro-star.scm"))
 #((Success)))


(def default-load.scm-path ".gambc/load.scm")

(def load.scm-extract-forms-yes
     '((begin recurse)
       (c/load 0)
       (i/load 0)
       (/load 0)
       (load 0)))

(def load.scm-extract-forms-no
     '((include)
       (quote)
       (define)))

(def (load.scm-extract sexpr tail)
     (mcase
      sexpr
      (pair?
       (let-pair
	((head rest) (source-code sexpr))
	(let ((head (-> symbol? (source-code head))))
	  (let ((q (lambda (lis)
		     (assq head lis))))
	    (let ((yes (q load.scm-extract-forms-yes))
		  (no (q load.scm-extract-forms-no)))
	      (if (if yes
		      (if no
			  (error "symbol in both yes and no lists:"
				 head)
			  #t)
		      (if no
			  #f
			  (error "symbol unknown:"
				 head)))
		  ;; extract
		  (xcase (cadr yes)
			 ((recurse)
			  (fold-right load.scm-extract
				      tail
				      rest))
			 ((0)
			  (cons (car rest) tail)))
		  ;; ignore
		  tail))))))))

(def (load.scm-files #!optional (load-path default-load.scm-path))
     (map (C string-append _ ".scm")
	  (load.scm-extract (cons 'begin (call-with-input-file load-path read-all))
			    '())))

(def (check-load.scm #!optional (all? #t) (load-path default-load.scm-path))
     (let ((modulepaths
	    (if all?
		(load.scm-files load-path)
		(filter (C string-starts-with? _ "lib/")
			(load.scm-files load-path)))))
       (vector (length modulepaths)
	       (modulepaths-satisfying? modulepaths))))

