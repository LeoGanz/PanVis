# panvis

install hpack with `stack install hpack` if missing

use `hpack` to update panvis.cabal based on changes in package.yaml (executed automatically by stack build)

if missing dependencies: `stack install`

**build with `stack build`**

run with `stack exec panvis-exe`

A GHCi session with the dependencies of the project can be startet with `stack ghci`
