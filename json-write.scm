;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 jclass
	 (list-util-1 map/last?)
	 string-bag)

(export (method .json-string)
	(method .json-string-bag)
	json-display)

;; https://datatracker.ietf.org/doc/rfc4627/?include_text=1

;; JSON can represent four primitive types (strings, numbers, booleans,
;; and null) and two structured types (objects and arrays).

;; A string is a sequence of zero or more Unicode characters [UNICODE].

;; An object is an unordered collection of zero or more name/value
;; pairs, where a name is a string and a value is a string, number,
;; boolean, null, object, or array.

;; An array is an ordered sequence of zero or more values.


;; Using native Scheme objects.

;; Strings

(def. (string.json-string v)
  ;; XXX correctness?
  (object->string v))

(def. string.json-string-bag
  string.json-string)


;; Numbers

(def json-representable-number?
     ;; XXX is this correct and complete?
     real?)

(def. (json-representable-number.json-string v)
  (number.string v))

(def. json-representable-number.json-string-bag
  json-representable-number.json-string)


;; Booleans

(def. (boolean.json-string v)
  (if v
      "true"
      "false"))

(def. boolean.json-string-bag
  boolean.json-string)


;; Nulls

;; #f would be ambiguous with booleans. void would be bad.  The Maybe
;; type is bad since it loses data in the conversion.  Thus need a new
;; value (sigh).

(jclass ((json-null _json-null))
	(def json-null:value (_json-null))
	(def (json-null) json-null:value))


(def. (json-null.json-string v)
  "null")

(def. json-null.json-string-bag
  json-null.json-string)


;; (when handling Nothing, also need to handle Just? But, loses information!)
;; (def. (Just.json-string v)
;;   (.json-string (Just.value v)))
;; (def. (Just.json-string-bag v)
;;   (.json-string-bag (Just.value v)))


;; "Objects"

;; The names within an object SHOULD be unique.

(def. (table.json-string-bag v)
  (list "{"
	(map/last?
	 (lambda (l? k.v)
	   (let-pair ((k v) k.v)
		     (assert ((either symbol? keyword?) k))
		     (list (string.json-string (.string k))
			   ":"
			   (.json-string-bag v)
			   (if l? "" ","))))
	 (table->list v))
	"}"))

(def. (table.json-string v)
  (string-bag->string (table.json-string-bag v)))


;; "Arrays"

(def. (pair-or-null.json-string-bag v)
  (list "["
	(list-join (map .json-string-bag v) ",")
	"]"))
;; could optimize, map-join or something.

(def. (pair-or-null.json-string v)
  (string-bag->string (pair-or-null.json-string-bag v)))



(TEST
 > (.json-string "")
 "\"\""
 > (.json-string "a")
 "\"a\""
 > (.json-string '())
 "[]"
 > (.json-string '((1 "a")))
 "[[1,\"a\"]]"
 > (.json-string (list (list 1.44 "a")
		       (json-null) #f (table '(a (122 "A")) '(b: 0)) #t))
 "[[1.44,\"a\"],null,false,{\"a\":[[122,\"A\"]],\"b\":[0]},true]")



(def (json-display v #!optional (port (current-output-port)))
     (string-bag-display (.json-string-bag v)))

