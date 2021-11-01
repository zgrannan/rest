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

Example commands (from repl):

``` sh
mkSetsRESTGraph RPO "example" "union(union(a,b), intersect(c,d)))" defaultParams
mkArithRESTGraph (Fuel 5) "example" "s(i) < s(j + s(i))" (withShowConstraints  defaultParams)
```

### Advanced

Generation of a graph is done from the REPL using the command 
`mk{theory}RESTGraph oc filename expr params` where

`theory` is the name of the theory containing rules (ie Arith, Sets, etc.)
`oc` is one of `RPO | KBO | Fuel Int`

For more information see the definition of `mkRESTGraph'`.

The file `Nat` includes the logic for default term pretty printer and parser.
Numbers are automatically converted to peano representation.

