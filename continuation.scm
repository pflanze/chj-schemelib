

(require easy)

(export continuation-return-no-winding
	continuation-graft-no-winding)



(def (continuation-return-no-winding #(continuation? cont)
				    . args)
     (apply ##continuation-return-no-winding cont args))

(def (continuation-graft-no-winding #(continuation? cont)
				    #(procedure? proc)
				    . args)
     (apply ##continuation-graft-no-winding cont proc args))


;; TODO: ##continuation-capture-aux
;; etc., for stack access..
