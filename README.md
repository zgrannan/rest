Haskell implementation for [REST: REwriting and Selecting Termination orderings][rest-paper]

[rest-paper]: https://s3.us-west-1.wasabisys.com/zg-public/paper.pdf


## Graph Generation

To run the REPL:

``` sh
stack ghci test/Main.hs
```

### General

Go into the REPL, then run the command in the desired section.
Graphs are output in the `graphs` folder.

The `dot` command-line tool is required.

Example command (from repl):

``` sh
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

