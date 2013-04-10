(requires gambit-interpreter-env
	  ;;keyword-util does not have a module file.
	  cj-env ;; *do-times;   keyword->symbol i've recreated here
	  srfi-1 ;; reverse!
	  )

;(namespace "")

(exports
 keyed->sxml
 sxml->keyed
 )
