# Object oriented programming

Object oriented here means just type hierarchies and virtual method
dispatch; no mutation of objects is currently provided (but can be
implemented if wished via boxes, or via custom mutators using
`vector-set!` (which is admittedly hacky, if used often enough it may
warrant adding a mutability declaration feature)), hence objects don't
(usually) encapsulate mutable state, and the programming style remains
a (mostly) purely functional one.

`class.scm` implements a class system similar to Java's, except as
indicated it doesn't provide for mutation. Functional setters are
automatically provided, they return a modified copy of the
object. Constructors are automatically defined with the same name as
the class name (since objects are (usually) immutable, there's no
point in prefixing their name with "make-" or "new-"). They expect the
values for all fiels as arguments, in the same order as the field
definitions (with fields from the parent class (if any) first), and
simply copy them into an instance of the corresponding
class. "Constructors" which need to do other things first need to be
written simply as functions/procedures which then call that
constructor. There is a constructor renaming feature in case there
really is a need to provide for a "constructor" under the same name as
the class but that does other things first (maybe to provide for
backwards compatibility after class fields have changed). 

`class.scm` is offering `defclass` and `defmethod`, as well as
`definterface` and `method`:

    (definterface point
      (method (area s) -> number?)
      (method (- a [point? b]) -> point?))

    (defclass (2dpoint x y)
      implements: point

      (defmethod (area s)
        (* x y)))

    (defclass (3dpoint z)
      extends: 2dpoint

      (defmethod (volume s)
        (* x y z)))

    (defclass ((4dpoint _4dpoint) [3dpoint? 3p] t)

      ;; A normal function definition, only reason to be placed inside the
      ;; class form is for the human reader to have it in sight.
      (def (4dpoint x y z t)
           (_4dpoint (3dpoint x y z) t))

      (defmethod (hypervolume s)
        (* (.volume 3p) t)))

    (TEST
     > (def 3v (3dpoint 10 20 30))
     > (=> 3v .y)
     20
     > (=> (2dpoint 10 20) .area)
     200
     > (=> 3v .area)
     200
     > (=> 3v .volume)
     6000
     > (=> (4dpoint 2 3 4 5) .hypervolume)
     120)

Those are simple wrappers (for the purpose of providing nice,
non-specific names, but might be in the way when mixing various object
systems) around the forms from `jclass.scm`, which defines the same
forms with "j" prefixes. The main feature this module implements
(which is inherited by `class.scm`) is provides is nested class
definitions, which defines subclassing without having to specify the
parent class:

    (jclass (again2dpoint x y)
      (jclass (again3dpoint z)))

    (TEST
     > (=> (again3dpoint 10 20 30) .y)
     20)

This module is implemented on top of `joo.scm`, which implements the
meat of the Java-inspired OO system. It is itself implemented on top
of `cj-struct.scm` (joo objects are just cj-struct instances),
`cj-typed.scm` (for type restriction syntax on field definitions), and
`dot-oo.scm` (which implements generics / method dispatch).

Interface definitions are currently purely decorative, but in the
future may provide for warnings about missing method implementations
or invalid types. (They might also be used for purposes like
auto-translating code to other languages like Java.)

There is (currently) no way to declare fields as private; the module
`export` form should possibly be used for that purpose, although
that's not implemented yet. Maybe export declaration at the definition
site will be useful, though, and may be considered.

`dot-oo.scm` defines `define.` (which is then shortened by `easy` to
`def.`), and is used in `joo.scm` for creating the `defmethod`
functionality. It creates generic functions, which dispatch according
to the (type of the) value in the first argument position.
  
    (def. (list.frob l a) (cons (string-append "frob" a) l))
    (def. (string.frob s a) (string-append "frob" a s))

    (TEST
     > (list.frob '(1 2 3) "foo")
     ("frobfoo" 1 2 3)
     > (string.frob "1 2 3" "foo")
     "frobfoo1 2 3"
     > (.frob "1 2 3" "foo")
     "frobfoo1 2 3"
     > (.frob '(1 2 3) "foo")
     ("frobfoo" 1 2 3))

In the above, `.frob` is the generic function, where `string.frob` and
`list.frob` are 

`dot-oo.scm` currently only supports dispatching on the first argument
of the generic. Dispatch is based on predicates functions, which can
be any Scheme function (as long as it is pure, there may also be
restriction when caring about performance in the future). `list.frob`
is called when `list?` returns true on the first argument.

Currently the dispatching search proceeds simply linear from the last
definition backwards (which means more general methods have to be
defined before more specific ones so as to not shadow the more
specific one!), but there are plans to lift this restriction and
performance drawback.


## Optimization

`dot-oo`, the module doing the dispatch used in `class`, isn't very
fast (although it's not too bad either):

* if there are many method definitions for the same generic, then the
  method search time will increase correspondingly. Remedies:

    * In call sites where the type of the object is known to the
      programmer, the name of the method implementer can be called
      directly; e.g. `(string.frob "1 2 3" "foo")` in the example
      above does not go through the dispatch algorithm and the cost is
      exactly the same as for a normal top-level function call.

    * There are plans to change from linear search to decompiling and
      creating optimized code to do the dispatch (turn into a
      predicate tree). Needs more work on the corescheme modules, and
      for proper decompilation (both to avoid having to depend on
      Gambit's compilation mode with debugging symbols) needs either
      to implement proper module and macro system from scratch, or
      introduce special forms which have to be used to define
      predicates.
      
    * Until that is ready, `dot-oo-optim-for` or `dot-oo-optim-for`
      from `dot-oo-optim.scm` can be used. Also, to avoid the need to
      allocate rest arguments, use the `%` macro in method calls (or
      `CALL.` from `dot-oo.scm`, XX should one of those die?)


## Widely used interfaces / methods

There is no consistent use of `definterface`, generics may just be
defined somewhere, hence those are often "interfaces" in an imaginary
sense only. 

Here are some methods which are used and extended widely, and the
module that introduces them:

`show.scm`, `(.show val)`: convert val back into Scheme code (an
s-expression) that generates it. Custom implementations 

`cj-exception-handler.scm`, `(.maybe-exception-message exn)`: custom
formatting for exception values in the repl.

`(.show-location s)` is introduced in `oo-util.scm` and used by the
`show-def` macro (via `current-show-def`, also set in `oo-util.scm`),
and is expected to print a line that is parsed by Emacs to jump to the
corresponding location. It falls back to calling `(.location s)` in
the default implementation (if that isn't defined, it fails).  TODO:
also use it in cj-exception-handler.scm alongside
.maybe-exception-message, also maybe only rely on .location
everywhere?

`(.first s)`, `(.map s fn)` etc.: API working on all sorts of
sequences.  See `oo-*.scm` modules. Not very consistent/complete.


## OO system introspection

### `dot-oo-introspection.scm`:

* `(can. genericname obj)` and `(CAN. genericname obj)` (the latter is
  a macro that doesn't need genericname (a symbol) to be quoted, and
  resolves it at compile time): these return `(maybe procedure?)`, the
  method implementor if available, i.e. whether genericname can work
  on obj and if so what it does.

* `(show-methods genericname)`: a macro (no need to quote
  genericname), shows all method implementors for the given generic.

* `(show-generics objs...)`: gives a list of generics which are
  implemented for all given objs (and on which type). `(show-generics*
  objs...)` gives the same but grouped by type. If those functions
  don't get any argument, they dispatch to `(show-all-generics)`,
  which gives a list of all generic names (and the list of the types
  for which they have definitions)

* `(show-method-statistics)`: if call statistics have been enabled via
  `(set! *dot-oo:method-stats* #t)`, shows the method call counts (for
  optimization purposes, see Optimization section).  `(set!
  *dot-oo:method-stats* 'location)` stores the call counts by caller
  location. `(set! *dot-oo:method-stats* 'continuation)` does the same
  but also stores away the continuation of the last call from each
  call site, so it can be inspected later. In both cases, you'll
  currently need to access the table shown in the output of
  `(show-method-statistics)` manually (e.g. call `.list` on its `#n`,
  then `,(v #n)` for the continuation shown). The statistics for a
  particular generic resets to 0 if any (new or existing) method of
  that generic is (re)defined.

* `(set! *dot-oo:method-trace* #t)`: enables showing of method calls
  as they happen.

### `joo-introspection.scm`: 

The `joo-type` type holds meta-information about a class. Various
methods are given to go from a class name or a joo instance to the
`joo-type`, and then to extract the various bits of meta-information
from that.

