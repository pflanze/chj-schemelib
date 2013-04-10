
(requires
 cj-env	;; warn, for debgging  ah and now thunk
 (cj-sxml with-sxml-element-attributes/else
	  sxml-element-attribute-ref
	  sxml-element:add-attributes-unless-present)
 (cj-stream stream-for-each)
 (cj-test TEST)
 )


(exports

 sxml>>html-fast
 sxml>>xhtml-fast
 sxml>>xml-fast

 sxml>>pretty-xml-file
 sxml>>pretty-xhtml-file

 sxml>>xhtml
 sxml>>html
 sxml>>xml
 
 
 sxml->html-string-fragment
 sxml->xml-string-fragment
 )

;; should have called the output functions  sxml>>xhtml  etc., rather. they are not converters!

;; sxml:write-xhtml>>
;; sxml:write-as-xhtml>>
;;what ever?..

;; write-sxml-as-xhtml

