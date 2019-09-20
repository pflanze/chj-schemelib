
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

- OO: see [OO](OO.md).

- show-def: see [OO#Widely used interfaces / methods](OO.md).


