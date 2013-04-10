(requires
 (cj-stream stream-map/filter stream-map stream-filter stream-map/filter/tail)
 ;;(cj-stream *stream-strict*)
 (srfi-1 append! fold-right)
 )

(exports
 
maybe-sxml-element-attribute-alist

sxml-attribute-ref
sxml-attribute-value-ref
sxml-attributes:ref
sxml-element-attribute-ref
sxml-element-attributes
sxml-element-name
sxml-element-body
sxml-element-bodytext
sxml-element-match-pathlist
sxml-element-search-subelement-with-name
sxml-element-search-subelement-with-name/attribute/value
sxml-element:add-attributes-unless-present
sxml-element?
sxml-elements-match-subpathlist
sxml-strip-whitespace
sxml-whitespace?

with-sxml-element
with-sxml-element-attributes
with-sxml-element-attributes/else
with-sxml-element/else

nbsp
)

(exports-on-request

 x-list-maybe-one-value
 x-list-one-value
 @with-sxml-element-attributes/else

 ;;?
 unbound
 ;;sollten raus?:
 char-whitespace?
 normalize-whitespace
 string-all-whitespace?
 string-count-chars//accessor
 string-count-leading-chars
 string-count-trailing-chars

 )