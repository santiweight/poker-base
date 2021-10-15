## poker-base

[![Hackage](https://img.shields.io/hackage/v/poker-base.svg)](https://hackage.haskell.org/package/poker-base)
[![Build Status](https://github.com/santiweight/poker-base/workflows/haskell-ci/badge.svg)](https://github.com/santiweight/poker-base/actions?query=workflow%3Ahaskell-ci) [![Hackage Deps](https://img.shields.io/hackage-deps/v/poker-base.svg)](http://packdeps.haskellers.com/reverse/{{name}})

## Description

Basic datatypes for writing code that handles poker (poker simulation, a poker website, solver etc.). This package is intended to be industry-ready (WIP).

Pull requests or suggestions (preferably created in an issue), are not just encouraged but highly requested! We are quick to respond to any help you might need :)

## Goal

To become Haskell's core de facto library for poker applications. We are working on this library so that your poker work can use `poker-base`, and its children packages, seamlessly.

Some principles that this library aims to uphold:
 - Targeted at intermediate Haskellers. This package will not shy away from using Haskell features that we deem right-for-the-job. However, we also make an effort to make `poker-base`'s API usable for the intermediate Haskeller.
 - Thin around the waist. This library aims to contain only such datatypes and functionality that is uncontroversial (~90% agree). If a datatype or API would be incompatible with some people's needs, then that functionality will not be included `poker-base`.
 - 100% test coverage/documentation. Still WIP.
 - Fast. This library is intended to be fast. However, note that API usability will come over speed always. Thankfully, Haskell has features such as `newtype` and `PatternSynonyms` which we use to hide implementation details.

## Libraries based on `poker-base`

 - [`poker-game`](https://github.com/santiweight/poker-game): a library for the rules of no-limit poker (not just holdem)
 - [`poker-histories`](https://github.com/santiweight/poker-histories): a library for parsing poker site hand histories. Currently supports PokerStars and Bovada.
 - [`poker-fold`](https://github.com/tonyday567/poker-fold): TODO @Tony
 - Various other private repos. If you are interested - please drop a line to the authors :)

## Contributing

See [Contributing.md](Contributing.md) for guidelines.

## Resources

See [Resource.md](Resources.md) for help regarding the rules of poker and resources for implementing poker applications.
