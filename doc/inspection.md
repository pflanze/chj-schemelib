
- macros:
    - prepend `expansion#` to the symbol that denotes the macro call, i.e.
    
            > (expansion#=> (.list "Agent 007 in 2 lives") (filter char-digit?) (.string))
            (.string (filter (.list "Agent 007 in 2 lives") char-digit?))

    - or, to see expansion of all macros (in all levels), but that
      only works on expressions that return a value, not
      e.g. definitions, and only when there's no error during compile
      time of the expression: e.g.
    
            > (expansion#=> (=> (.list "Agent 007 in 2 lives") (filter char-digit?)) (.string))
            (.string (=> (.list "Agent 007 in 2 lives") (filter char-digit?)))
            
       did not expand the inner use of =>, whereas expansion without the # does:
       
            > (expansion => (=> (.list "Agent 007 in 2 lives") (filter char-digit?)) (.string))
            (.string (filter (.list "Agent 007 in 2 lives") char-digit?))

- dot-oo.scm defines the basis of the `|def.|` forms, as well as
  `|def-method|` in `|jclass|`. It creates generic functions, which
  dispatch according to the (type of the) value in the first argument
  position (or said differently, the generic function finds a match
  for the first argument, amongst the methods that are "part" of the
  generic.
  
     (def. (list.frob l a b) (cons (string-append "frob" a) l))
     (def. (string.frob s a) (string-append "frob" a s))

  > (list.frob '(1 2 3) "foo")
  ("frobfoo" 1 2 3)
  > (string.frob "1 2 3" "foo")
  "frobfoo1 2 3"
  > (.frob "1 2 3" "foo")
  "frobfoo1 2 3"
  > (.frob '(1 2 3) "foo")
  ("frobfoo" 1 2 3)

