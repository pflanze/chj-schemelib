

(require easy
	 test)

(export continuation-return-no-winding
	continuation-graft-no-winding)



(def (continuation-return-no-winding #(continuation? cont)
				    . args)
     (apply ##continuation-return-no-winding cont args))

(def (continuation-graft-no-winding #(continuation? cont)
				    #(procedure? proc)
				    . args)
     (apply ##continuation-graft-no-winding cont proc args))



(TEST
 > (defparameter tp #f)
 > (def cnt 0)
 > (def (t)
	(continuation-capture
	 (lambda (exit)
	   (parameterize
	    ((tp 111))
	    (letv ((newtp newc)
		   (continuation-capture
		    (lambda (cont)
		      (continuation-return-no-winding exit cont))))
		  (warn "tp = "(tp))
		  ;;(tp newtp)
		  newc)))))
 > (def c (t))
 > (if (< cnt 3)
       (begin
	 (inc! cnt)
	 (warn "calling c; tp,cnt=" (tp) cnt)
	 (parameterize
	  ((tp cnt))
	  (continuation-return-no-winding c (values cnt c)))))
 ;; can't get it to do no winding. why is this, SGH????
 )



(TEST
 > (def cnt 0)
 > (def (t)
	(continuation-capture
	 (lambda (exit)
	   (dynamic-wind
	       (lambda ()
		 (warn "going in"))
	       (lambda ()
		 (letv ((signal newc)
			(continuation-capture
			 (lambda (cont)
			   (continuation-return exit cont))))
		       (warn "signal = " signal)
		       newc))
	       (lambda ()
		 (warn "going out"))))))
 > (def c (t))
 > (if (< cnt 3)
       (begin
	 (inc! cnt)
	 (warn "calling c; tp,cnt=" (tp) cnt)
	 (continuation-return c (values cnt c)))))

(TEST
 > (def cnt 0)
 > (def (t)
	(continuation-capture
	 (lambda (exit)
	   (dynamic-wind
	       (lambda ()
		 (warn "going in"))
	       (lambda ()
		 (letv ((signal newc)
			(continuation-capture
			 (lambda (cont)
			   (continuation-return-no-winding exit cont))))
		       (warn "signal = " signal)
		       newc))
	       (lambda ()
		 (warn "going out"))))))
 > (def c (t))
 > (if (< cnt 3)
       (begin
	 (inc! cnt)
	 (warn "calling c; tp,cnt=" (tp) cnt)
	 (continuation-return-no-winding c (values cnt c)))))



;; TODO: ##continuation-capture-aux
;; etc., for stack access..


