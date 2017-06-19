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
	 Result
	 (cj-functional complement)
	 (require path-string.relation))

(export lib
	mydb
	check-load.scm
	loadorder-in-dirs
	dependency-graph-in

	#!optional
	path-string.topo-relation
	modulepaths-tsort
	modulepaths-in-dir
	modulepaths-in-dirs
	(class requires)
	modulepaths-satisfying?
	load.scm-files)


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

(TEST
 > (require:include? 'foo--include)
 #t
 > (require:include? 'foo--includes)
 #f
 > (require:include? 'foo-include)
 #f
 > (require:include? 'foo)
 #f)



(def (path-string.topo-relation p)
     (path-string.relation p topo-relation))

(TEST
 ;; well those are evil of course, will break upon module changes
 > (path-string.topo-relation "lib/require-util.scm")
 #((topo-relation)
   require-util
   (easy test tree-util cj-io-util tsort Result cj-functional require))
 ;; > (path-string.topo-relation "tsort.scm")
 ;; #((topo-relation) tsort (easy test alist))
 )

(def (modulepaths-tsort paths)
     (let* ((rs (map path-string.topo-relation paths))
	    (z (map cons (map .name rs) paths)))
       (map (C symbol-alist-ref z _) (topo.sort* rs))))


;; path before normalization
(def (require-util:modulepath-ignore? path)
     (or (string-starts-with? path "Attic/")
	 (string-starts-with? path "Trash/")
	 (string-starts-with? path "trash/")
	 (string-starts-with? path "trash/")
	 (string-starts-with? path ".gambc/")))

(def require-util:real-modulepath?
     (complement require-util:modulepath-ignore?))

(def (modulepaths-in-dir dir #!optional (tail '()) normalize?)
     (parameterize
      ((current-directory dir))
      (map/tail (if normalize?
		    path-normalize
		    (lambda (p)
		      (path-append dir p)))
		tail
		(filter require-util:real-modulepath?
			(xcall-with-input-process
			 (list path: "git" arguments: '("ls-files" "*.scm"))
      
			 read-lines)))))


(def (modulepaths-in-dirs dirpaths #!optional (tail '()))
     ;;(flatten (map modulepaths-in-dir dirpaths))
     (fold-right modulepaths-in-dir tail dirpaths))

(def (loadorder-in-dirs . dirpaths)
     (modulepaths-tsort (modulepaths-in-dirs dirpaths)))

(def (lib)
     (loadorder-in-dirs "lib"))
(def (mydb)
     (loadorder-in-dirs '("lib" "mydb")))

;; Dependency graph

;; dependencies as a string bag describing 'dot' code; run as
;; e.g. (print (dependency-graph-in "lib")) then pass the output to
;; e.g. "dot -Tpng output.dot > output.png"
(def (dependency-graph-in . dirpaths)
     (def show (comp object->string symbol.string))
     (list "digraph {\n"
	   (map (lambda (modulepath)
		  (path-string.relation
		   modulepath
		   (lambda (name names)
		     (list "  "
			   (show name) " -> { "
			   (map (lambda (name)
				  (list (show name) " "))
				names)
			   "}\n"))))
		(modulepaths-in-dirs dirpaths))
	   "}\n"))


;; a single-dependency representation
(class requires
       (struct name dependency))

(def (modulepaths-satisfying? paths) -> Result?
     (call/cc
      (lambda (return)
	(let* ((all-rs (map path-string.topo-relation paths)))
	  ;; nw how. wat's already loaded  walk through.
	  (let lp ((rs all-rs)
		   (loaded '()))
	    (if (null? rs)
		(Ok #t) ;; bah, had this nice (Success) before.
		(let-pair
		 ((r rs*) rs)
		 (for-each
		  (lambda (d)
		    (unless (memq d loaded)
			    (return (Error (requires (.name r) d)))))
		  (.deps r))
		 (lp rs*
		     (cons (.name r) loaded)))))))))

(TEST
 > (modulepaths-satisfying? '("lib/Maybe.scm" "lib/easy.scm"))
 #((Error) #((requires) Maybe easy))
 > (modulepaths-satisfying? '("lib/easy-1.scm" "lib/Maybe.scm"))
 #((Error) #((requires) easy-1 define-macro-star))
 > (modulepaths-satisfying? '("lib/cj-source.scm"
			      "lib/define-macro-star.scm"))
 #((Ok) #t))


(def default-load.scm-path ".gambc/load.scm")

(def load.scm-extract-forms-yes
     '((begin recurse)
       (if recurse-if)
       (let recurse-let)
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
			 ((recurse-if)
			  (let ((test (car rest))
				(then (cadr rest))
				(perhaps-else (cddr rest)))
			    (if (eval test)
				(load.scm-extract then tail)
				(if (pair? perhaps-else)
				    (load.scm-extract (car perhaps-else) tail)
				    tail))))
			 ((recurse-let)
			  (let ((binds (car rest))
				(body (cdr rest)))
			    ;; simply ignore binds (for now), rely on
			    ;; still only using /load, i.e. only
			    ;; binding /load is allowed, I mean, still
			    ;; only /load works so only binding that
			    ;; makes sense.
			    (fold-right load.scm-extract
					tail
					body)))
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

