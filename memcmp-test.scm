;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 memcmp
	 test)

(TEST
 > (memcmp:substring=? "foo" 0 "bar" 0 0)
 #t
 > (memcmp:substring=? "foo" 0 "bar" 0 1)
 #f
 > (memcmp:substring=? "foo" 0 "far" 0 1)
 #t
 > (memcmp:substring=? "foo" 0 "far" 0 2)
 #f
 > (memcmp:substring=? "foo" 1 "oof" 0 2)
 #t
 > (memcmp:substring=? "foo" 1 "oof" 0 1)
 #t
 > (memcmp:substring=? "foo" 2 "oof" 0 1)
 #t
 > (memcmp:substring=? "foo" 2 "oof" 1 1)
 #t
 > (memcmp:substring=? "foo" 2 "oof" 2 1)
 #f
 > (memcmp:substring=? "foof" 3 "oof" 2 1)
 #t
 > (memcmp:substring=? "foof" 3 "oof" 2 2)
 ;; since len 2 goes past the strings so the requested region isn't
 ;; covered; XX should it return an error value instead? False but
 ;; error, ever ?
 #f
 > (memcmp:substring=? "foofa" 1 "oof" 0 3)
 #t
 > (memcmp:substring=? "foofa" 1 "oof" 0 4)
 #f
 > (memcmp:substring=? "foofa" 2 "oofx" 1 2)
 #t
 > (memcmp:substring=? "foofa" 2 "oofx" 1 3)
 #f)


(TEST
 > (%try (memcmp:substring=? "foofa" 2 "oofx" 1 ""))
 (exception text: "memcmp:substring=?: need string, natural0 (start), string, natural0 (start), natural0 (len): \"foofa\" 2 \"oofx\" 1 \"\"\n")
 > (%try (memcmp:substring=? "foofa" 2 "oofx" 1 -1))
 (exception text: "memcmp:substring=?: need string, natural0 (start), string, natural0 (start), natural0 (len): \"foofa\" 2 \"oofx\" 1 -1\n")
 > (%try (memcmp:substring=? "foofa" 2 "oofx" 1. 1))
 (exception text: "memcmp:substring=?: need string, natural0 (start), string, natural0 (start), natural0 (len): \"foofa\" 2 \"oofx\" 1. 1\n"))

