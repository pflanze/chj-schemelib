(requires gambit-interpreter-env
	  ;;cj-queue
	  cj-alist
	  cj-env
	  (cj-list-util map-iota))

(compile #t)

(exports

 ;; many SIGxxx names. how to export those automatically?
 SIGCHLD
 SIGPOLL
 ;;...
 SIGIO ;; = SIGPOLL

 signal-number->name ;; takes int. dies on non-existing signals.[could this be made part of type sys?]
 
 interrupt-install-handler! ;; (interrupt-install-handler! signal-number handler)
 interrupt-remove-handler! ;; (interrupt-remove-handler! signal-number)
 )

(exports-on-request

 make-sigqueue
 sigqueue-empty?
 sigqueue-endpos
 sigqueue-full?
 sigqueue-overflow
 sigqueue-overflow-reset!
 sigqueue-positions
 sigqueue-startpos
 sigqueue-take!
 sigqueue-usage
 sigqueue-add!
 sigqueue-remove!
 sigqueue-ref-signo
 ;; constant-from-c: 
 SIGQUEUE_ERROR
 SIGQUEUE_ERROR2
 SIGQUEUE_SUCCESS
 ;; ?:
 sig-errstr

 ;; ~lowlevel handling:
 init
 call-with-locks
 global_queue
 signal-take!
 interrupt-dispatch
 sig-lock!
 sig-unlock!
 sig-locked?

 handlers
 handlers-set! ;;(this is for alist-set!)

 )

 ;; compiletime: 
 ;; make-gen-code
