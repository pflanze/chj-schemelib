;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

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


(def (modulepaths-in-dir dir #!optional (tail '()))
     (parameterize
      ((current-directory dir))
      (map/tail path-normalize
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

(def (load.scm-files load-path)
     (map (C string-append _ ".scm")
	  (filter (both string?
			(complement (C string-ends-with? _ ".scm")))
		  (flatten
		   (call-with-input-file load-path read-all)))))

(def (check-load.scm all? #!optional (load-path default-load.scm-path))
     (let ((modulepaths
	    (if all?
		(load.scm-files load-path)
		(filter (C string-starts-with? _ "lib/")
			(load.scm-files load-path)))))
       (vector (length modulepaths)
	       (modulepaths-satisfying? modulepaths))))

