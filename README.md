Haskell implementation for [REST: REwriting and Selecting Termination orderings][rest-paper]

[rest-paper]: https://arxiv.org/abs/2202.05872

This package provides functions to determine if a sequence of terms has a
decreasing order. This is useful for automatic theorem provers that need to
decide when to stop exploring a particular path in a proof.

The main interface is `Language.REST.OCAlgebra`.

```Haskell
-- | Ordering Constraint Algebra: Operations to determine if there is a
--   well-quasi ordering that makes a sequence of terms non-increasing.
--
-- @c@ is a type representing ordering constraints.
--
-- @a@ is the type of the terms in a sequence.
--
-- @m@ is a monad that allows to perform computations, such as checking the
-- satisfiability of ordering constraints.
--
data OCAlgebra c a m = OCAlgebra
  { -- | Checks if the constraints are satisfiable
    isSat  :: c -> m Bool
    -- | @refine c t u@ strengthens @c@ to permit @t >= u@
  , refine :: c -> a -> a -> m c
    -- | Allows any non-increasing ordering
  , top :: c
    -- | @union c0 c1@ allows any of the orderings permitted by @c0@ or @c1@
  , union  :: c -> c -> c
    -- | @c1 `notStrongerThan` c2 == True@ if @c1@ permits all orderings allowed
    -- by @c2@.
  , notStrongerThan :: c -> c -> m Bool
  }
```

Some implementations of `OCAlgebra`s are provided in `Language.REST`. They
communicate with an SMT solver to determine the satisfiability of constraints.

## Graph Generation

This package also provides functions to render trees generated with rewrite
rules, and pruned with OCAlgebras.

You will need to make the `dot` tool from the `graphviz` package available in
your PATH.

To run the REPL:

``` sh
cabal repl rest-rewrite:test:rest
```

Then run a command that will place a graphs in the `graphs` folder.

``` sh
mkArithRESTGraph (Fuel 5) "example" "s(i) < s(j + s(i))" (withShowConstraints  defaultParams)
```

Generation of a graph is done from the REPL using the command

    mk{theory}RESTGraph oc filename expr params

where `theory` is the name of the theory containing rules (ie Arith, Sets, etc.)
`oc` is one of `RPO | KBO | Fuel Int`

For more information see the definition of `mkRESTGraph'`.

The file `Nat` includes the logic for default term pretty printer and parser.
Numbers are automatically converted to peano representation.

