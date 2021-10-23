# Contributing

We respect and encourage contributions of any kind. If you love poker or Haskell, there's plenty of ways to contribute and enjoy both.

To report bugs, please use the [issue](https://github.com/santiweight/poker-base/issues) tracker.

We especially appreciate [pull requests](https://github.com/santiweight/poker-base/pulls) and are friendly to newcomers, experienced or otherwise.

For proposing API changes and enhancement ideas, it's a good idea to discuss it as an issue first, and then follow the guidelines below.

## Getting Started

We use the following tools in the library to help with testing and style:

- [tasty-discover](https://hackage.haskell.org/package/tasty-discover) as a runner for the [tasty](https://hackage.haskell.org/package/tasty) test framework.
- [ormolu](https://hackage.haskell.org/package/ormolu) to automate code formatting consistency.
- [hlint](https://hackage.haskell.org/package/hlint) for code linting, and
- [cabal-docspec](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) as a doctest runner.

They can be installed with:

`cabal install tasty-discover ormolu hlint --overwrite-policy=always`

Note that, if you use `haskell-language-server`, you may already have `ormolu` and `hlint` installed which are the default linters for Haskell.

`cabal-docspec` is not on hackage but can be installed as follows:

```
git clone https://github.com/phadej/cabal-extras
cd cabal-extras/cabal-docspec
cabal install cabal-docspec:exe:cabal-docspec --allow-newer --overwrite-policy=always
```

## Pull Request checklist

As a base library, a `poker-base` has to be kept neat, tidy, documented and tested. To that end, please ensure that the library:

- builds with `cabal build && cabal test` with no warnings. Feel free to add file pragmas or edit the `poker-base.cabal` if you think the warning should be ignored.
- passes `ormolu --cabal-default-extensions --mode check $(git ls-files '*.hs')`.
- passes `hlint .`
- passes `cabal v2-build && cabal-docspec --check-properties`

If you are requesting a change that adds new functionality or affects behaviour, please:

- check and add [doctests](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md#writing-doctests)
- add [tasty](https://hackage.haskell.org/package/tasty) unit tests, including [QuickCheck](https://hackage.haskell.org/package/tasty-quickcheck) properties.
- include [haddocks](https://haskell-haddock.readthedocs.io/en/latest/) that will be useful for other users.
- update the change log for non-trivial changes.

If anything is not working for you... don't waste your time! Open an issue and ask the authors!