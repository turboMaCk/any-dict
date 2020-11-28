# AnyDict

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2FturboMaCk%2Fany-dict%2Fbadge&style=flat)](https://actions-badge.atrox.dev/turboMaCk/any-dict/goto)

This library implements a thin wrapper around implementation
of a dictionary from the core library to make it possible to
use it in a fairly type-safe way with keys of any type,

It solves the same problem as [elm-all-dict](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest) just in a very much different way
without any Kernel (Native) code and on top of existing and well tested
`Dict` type.

API mirrors the standard `Dict` exactly where possible.

Some parts of the documentation are stolen directly from [`elm-lang/core`](http://package.elm-lang.org/packages/elm-lang/core/latest).
