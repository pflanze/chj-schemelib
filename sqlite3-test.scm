;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         sqlite3
         test)


(TEST
 > (def db-path "foo") ;; XX in-memory via #f 
 > (delete-file db-path)
 > (def db (sqlite3-open db-path))
 > (define-macro (TRY th)
     `(with-exception-catcher .maybe-exception-message (& ,th)))
 > (TRY (.prepare db "select abc"))
 ("sqlite3-error/message" SQLITE_ERROR "no such column: abc")
 > (TRY (.prepare db "create table foo2 (numeric id, text name"))
 ("sqlite3-error/message" SQLITE_ERROR "near \"name\": syntax error")
 > (TRY (.prepare db "create table foo2 ()"))
 ("sqlite3-error/message" SQLITE_ERROR "near \")\": syntax error")
 > (def createfoo2 (.prepare db "create table foo2 (id numeric, name text)"))
 > (.execute createfoo2)
 #!void
 > (TRY (.execute createfoo2))
 ("sqlite3-error/message" SQLITE_ERROR "table foo2 already exists")
 > (def insfoo2 (.prepare db "insert into foo2 values(?, ?)"))
 > (.bind insfoo2 1 7)
 #!void
 > (.bind insfoo2 2 "Happy")
 #!void
 > (.execute insfoo2)
 #!void
 > (.reset insfoo2)
 > (.bind insfoo2 1 8)
 > (.bind insfoo2 2 "Happy\0Wörld")
 > (.execute insfoo2)
 > (def getfoo2 (.prepare db "select * from foo2"))

 ;; Purely imperatively:
 > (.step getfoo2)
 SQLITE_ROW
 > (.get-row getfoo2 list)
 (7 #u8(72 97 112 112 121)) ;; "Happy"
 > (.step getfoo2)
 SQLITE_ROW
 > (.get-row getfoo2 list)
 (8 #u8(72 97 112 112 121 0 87 195 182 114 108 100)) ;; "Happy\0Wörld"
 > (.step getfoo2)
 SQLITE_DONE

 ;; Via get-rows:
 > (.reset getfoo2)
 > (def rs (.get-rows getfoo2))
 > (promise? rs)
 #t
 > (.first rs)
 (7 "Happy")
 > (.second rs)
 (8 "Happy\0Wörld")
 > (=> rs (.map first) .list)
 (7 8)
 ;; Can still get the u8vectors:
 > (=> getfoo2 .reset (.get-rows list) .second)
 (8 #u8(72 97 112 112 121 0 87 195 182 114 108 100))
 
 ;; A statement handle can only be used by one stream at the same
 ;; time; check detection:
 > (.reset getfoo2)
 > (def rs (.get-rows getfoo2))
 > (promise? rs)
 #t
 > (.first rs)
 (7 "Happy")
 > (=> getfoo2 .reset (.get-rows list) .second)
 (8 #u8(72 97 112 112 121 0 87 195 182 114 108 100))
 > (TRY (.second rs))
 ("sqlite3-statement.get-rows: no. concurrent side-effects on this statement:"
  3)

 > (=> db (.prepare "select id from foo2") .get-rows .list)
 ((7) (8))
 > (=> db (.prepare "select id from foo2") (.get-rows id) .list)
 (7 8)
 > (=> db (.prepare "select length(name) from foo2") (.get-rows id) .list)
 (5 5) ;; wow "returns the number of characters (not bytes) .. prior to the first NUL character" (and can't find a function that gives the full length)
 > (=> insfoo2 (.send-rows '((10 "Fun ten")
                             (11 "Good number")
                             ("Foo" 4444))))
 #!void
 > (=> db (.prepare "select length(name),id from foo2") .get-rows .list)
 ((5 7) (5 8) (7 10) (11 11) (4 "Foo"))
 > (=> db (.prepare "select substr(name, 2,5) from foo2") .get-rows .list)
 (("appy") ("appy") ("un te") ("ood n") ("444")) ;; same, stops at NUL, crazy?
 > (=> db (.prepare "select substr(name, 7,66) from foo2") .get-rows .list)
 (("") ("") ("n") ("umber") (""))
 > (=> db (.prepare "select * from foo2") .get-rows .list)
 ((7 "Happy")
  (8 "Happy\0W\366rld")
  (10 "Fun ten")
  (11 "Good number")
  ("Foo" "4444"))
 )

