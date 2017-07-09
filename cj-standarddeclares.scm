(declare (block)
	 (standard-bindings)
	 (extended-bindings))

;; Make overridden standard ops still be overridden using namespacing
;; hack:

(##namespace ("cj-struct#" vector?))

