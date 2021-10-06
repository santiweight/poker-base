## poker-base

[![Hackage](https://img.shields.io/hackage/v/poker-base.svg)](https://hackage.haskell.org/package/poker-base)
[![Build Status](https://github.com/santiweight/poker-base/workflows/haskell-ci/badge.svg)](https://github.com/santiweight/poker-base/actions?query=workflow%3Ahaskell-ci) [![Hackage Deps](https://img.shields.io/hackage-deps/v/poker-base.svg)](http://packdeps.haskellers.com/reverse/{{name}})

## Description

Basic datatypes for writing code that handles poker (poker simulation, a poker website, solver etc.). Most datatypes and their functions are well-tested; untested functions are generally documented. The goal is to eventually reach 100% code coverage and for this package to be kept to a high industrial quality.

Pull requests or suggestions (preferably created in an issue), are not just encouraged but highly requested! I should be highly responsive for the foreseeable future.

## Goal

To become the standard (read "only existing") Haskell poker library for basic types. Currently there are 4/5 unmaintained or bit-rotting libraries that can be revived and potentially work toegether. Some issues that present themselves:
 - Each library rewrites its own types with often compeletely identical or easily-unifiable types. Everyone loses when that's the case!
 - Each library comes with a lot of implementation for the thing it's trying to achieve (no library I know of is a basic poker types library). This makes it hard for libraries to import each other since you import someone else's project, not just basic types.
 - Those libraries have a lot of opinionated naming/functionality.
 - Some of the libraries are no longer maintained.

This library aims to solve those issues by keeping its scope incredibly tight and as unopinionated as possible. This library pretty much _only_ contains basic poker types (aimed at Texas Hold'Em) that _anyone_ can use in a project. This includes mildly-runtime/memory-sensitive applications such as webservers or poker equity calculators. However, I don't intend on making this library a fits-all. In particular, I will only incorporate optimisations that allow for this library to be pleasant to use for an intermediate Haskeller, or a beginner with some help. Perhaps one day someone can make a `poker-fast` library that will accommodate your Speedy-Gonzalez needs!

This library also doesn't shy away from some "not simple" Haskell. For example, there are a number of usages of ViewPatterns, TypeApplications, GADTs, and PatternSynonyms. Some of these are used to interact with [`safe-money`](https://hackage.haskell.org/package/safe-money), which seems like a very reasonable library for what we need. These design choices have all seemed fitting for me (some are not otherwise representable). But I'm definitely open to simplifying some of these usages if people feel the need! While I think these patterns make some code harder to read, most of these complex-Haskell usages are isolated to handling bet quantities, which should use as much safety as it can get!

## Current State of Library - What Needs to be Done

Currently the library is going through a final revision phase, in order to make sure that the current API is stable and maintainable. Some major issues (for which PRs or help would be greatly welcomed) include:
 - Use a more optimal representation of the Card type, such as in https://github.com/ghais/poker. If you are ghais - let's work together :) Many type declarations in this library should use UNPACK pragmas or more efficient representations (for example, `data Suit = Suit Bit Bit`???).
 - Continue to iterate on documentation.
 - Improve some APIs on types. Some usage of private type constructors are probably misuses, for example. Some creator-functions are also either not-yet-revised or untested.
 - Avoid using Enum as a fake attempt at optimisation. I'm only lying to myself.
 - Documentation with reasonable examples

## Usage in Other Libraries

I use this library in a couple different little projects I work on at home. Some projects that I'll be open-sourcing in the near future include:
 - poker-game: a library for the rules of poker
 - poker-history: a library for parsing poker site hand histories. I currently support PokerStars and Bovada - if you have any hand databases for other sites, I will happily support them.
 - poker-reflex: a collection of different [reflex-frp](https://reflex-frp.org/) JS widgets for stuff like showing a range or poker table with statefulness.

I also use this library for some private work that I use to study poker, when I get the chance, which is not often :(, but I'm always excited to talk about it!

## Let's Collaborate!

I have a bunch of work that I've worked on on-and-off for a while now and I figure now's the time to get it all out there! Some stuff I am interested in:
 - abstracting GTO poker solvers' solutions (using ML and other techniques).
 - UI-based DSLs and Haskell DSLs for querying poker game trees, especially against databases of online hands.
 - probably anything poker-related if you get me thinking about it LOL

## Contributing

See [Contributing.md](Contributing.md) for guidelines.

