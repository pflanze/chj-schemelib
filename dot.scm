;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Display Scheme data structures as directed graphs using the "dot"
;; tool from graphviz via "display" from imagemagick.

;; apt install graphviz imagemagick

(require easy
	 bag
	 (cj-io-util xsystem))

(export display-dot)


;; Process random Scheme data in two steps:

;; 1. convert to a bag of dot objects via .dot-bag (which contain the
;;    original objects)

;; 2. convert that to a bag of strings via dot-bag.string-bag (which
;;    calls dot's .string-bag methods, which use .dot-name to turn the
;;    objects into a string representation), print that to a file and
;;    run "display" on it.


(def string-bag? (bag-of string?))

(def. (any.dot-name v)
  ;; XX are there any differences from Scheme to C strings? Well,
  ;; unicode, right?
  (object->string ;; <- string -> C style string, really
   (object->string v)))

(def. (pair.dot-name v)
  (list "\"pair #" (number.string (object->serial-number v)) "\""))

(def. (struct.dot-name v)
  (list "\"" (object->string (struct-type-name v))
	" #" (number.string (object->serial-number v)) "\""))


;; https://graphviz.gitlab.io/_pages/doc/info/lang.html

(definterface dot
  (method (string-bag s) -> string-bag?)


  (def dot-bag? (bag-of dot?))

  (def. (dot-bag.string-bag b)
    ;; XX digraph needed for -> but only allows that?
    (list "digraph foo {\n"
	  ;; "\tordering=out;\n"
	  ;; "\trankdir=;\n"
	  ;; "\trank=same;\n"
	  "\tranksep=0.4;\n" ;; makes it a bit less tall
	  ;; "\tmindist=3.4;\n" no effect
	  ;; "\tepsilon=0.0001;\n" only for neato
	  (map .string-bag (bag->list b))
	  "}\n"))


  (defclass (dot-leaf v)

    (defmethod (string-bag s) -> string-bag?
      ;; (list "  " (.dot-name v) ";\n")
      ;; or, omit?
      ""))

  
  (defclass (dot-> object [list? links])

    (defmethod (string-bag s) -> string-bag?
      (list
       ;; formatting for the object itself
       (list "\t"
	     (.dot-name object)
	     " [ fontsize=7, shape=record ];\n")
       ;; and the pointers to the next
       (list "\t"
	     (.dot-name object)
	     " -> { "
	     (list-join (map .dot-name links)
			'(" "))
	     " } [ color=red ];\n")))))


(def. (dot-bag.string l)
  (=> l dot-bag.string-bag bag->string))


;; XX tmpfile?
(defparameter dot:tmp-path ".dot-scm.dot")

(def. (dot-bag.display l)
  (let ((path (dot:tmp-path)))
    (=> l
	dot-bag.string-bag
	(putfile path))
    (future (xsystem "display" path))))


(def. (any.dot-bag v)
  (dot-leaf v))

(def. (pair.dot-bag v)
  (list (dot-> v (list (car v) (cdr v)))
	(.dot-bag (car v))
	(.dot-bag (cdr v))))

(def. (null.dot-bag v)
  '())

(def. (vector.dot-bag v)
  (cons (dot-> v (vector.list v))
	(vector.map-list v .dot-bag)))

(def. (struct.dot-bag v)
  (let ((vs (struct-values v)))
    (cons (dot-> v vs)
	  (ilist.map vs .dot-bag))))


(def (display-dot v)
     (.display (.dot-bag v)))

