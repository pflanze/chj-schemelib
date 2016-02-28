;; 'parse them', sort them, kr(ypton)

(require easy
	 test
	 (tree-util flatten)
	 tsort
	 Status)


(def (require-decl.modulename v)
     (mcase v
	    (symbol?
	     (source-code v))
	    (pair?
	     (source-code (car (source-code v))))))

;; module-basename (vs. full module name in the future?)
(def (path-string.modulename p)
     (basename p ".scm"))


(def modules-without-require-forms
     '(cj-source
       define-macro-star
       dummy-module
       vector-util-1

       ;; lib/mod/ :
       config-example
       gambit
       imperative-load-tree
       lib
       mod
       monad
       monadic-load-tree
       remote
       usersyntax))

(def (path-string.topo-relation p)
     (let ((form (call-with-input-file p read))
	   (mname (string.symbol (path-string.modulename p))))
       (mcase form
	      (`(require . `rest)
	       (topo-relation mname
			      (map require-decl.modulename rest)))
	      (else
	       (if (memq mname modules-without-require-forms)
		   (topo-relation mname '())
		   (error "file does not have a require form as its first form:"
			  p))))))

(TEST
 ;; well those are evil of course, will break upon module changes
 > (path-string.topo-relation "require.scm")
 #(topo-relation require (easy test tsort Status))
 ;; > (path-string.topo-relation "tsort.scm")
 ;; #(topo-relation tsort (easy test list-ref))
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
 #(Failure #(requires Maybe easy))
 > (modulepaths-satisfying? '("lib/easy.scm" "lib/Maybe.scm"))
 #(Failure #(requires easy define-macro-star))
 > (modulepaths-satisfying? '("lib/define-macro-star.scm"))
 #(Success)
 )



(def (load.scm-files)
     (map (C string-append _ ".scm")
	  (filter string?
		  (flatten
		   (call-with-input-file ".gambc/load.scm" read-all)))))

(def (check-load.scm)
     (modulepaths-satisfying?
      (filter (both (either (C string-starts-with? _ "lib/")
			    (C string-starts-with? _ "mydb/"))
		    (complement (C string-ends-with? _ ".scm")))
	      (load.scm-files))))

