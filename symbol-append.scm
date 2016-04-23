;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

(require)

(define (symbol-or-string->string v)
  (cond ((string? v)
	 v)
	((symbol? v)
	 (symbol->string v))
	(else (error "invalid type:" v))))

(define (symbol-append . vals)
  (string->symbol
   (apply string-append
	  (map 
	   symbol-or-string->string
	   vals))))

