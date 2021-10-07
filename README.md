Haskell implementation for REST

To run the REPL:

``` sh
stack ghci
```

## Graph Generation

### General

Go into the REPL, then run the command in the desired section.
Graphs are output in the `graphs` folder.

The `dot` command-line tool is required.

See the Sets section for some simple commands to try stuff out.

### Sets

Example commands (from repl):

``` sh
mkSetsRESTGraph Tree AC.adtOC "example" "union(union(a,b), intersect(c,d)))"
mkSetsRESTGraph Tree AC.adtOC "example2" "union(t1,t2)"
```

### Advanced

Generation of a graph is done from the REPL using the command 
`mk{theory}RESTGRAPH type oc fname term` where

`theory` is the name of the theory containing rules (ie Arith, etc.)
`type` is one of `Tree | DAG | Min`. Just use `Tree`. 
`oc` is the implemenation of ordering constraints. Recommend to us `AC.adtOC`,
which uses Z3 to solve the constraints. `SC.strictOC` may result in cleaner
constraints when rendering constraints, though.

To change the pretty printing options, see the definition of `mkRESTGraph'`.
This can control how terms, rewrite rules, and constraints are rendered.

The file `RESTDot` has the logic for how the graph is rendered, for example if
you want to hide rejected edges here is the spot.

The file `Nat` includes the logic for default term pretty printer and parser.
Numbers are automatically converted to peano representation.

