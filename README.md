# AnyDict

[![Build Status](https://github.com/turboMaCk/any-dict/actions/workflows/test.yml/badge.svg)](https://github.com/turboMaCk/any-dict/actions/workflows/test.yml)

This library implements a thin wrapper around
dictionaries from core library adding support for keys of any type.

It solves the same problem as [elm-all-dict](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest) did just in a very much different way
without any Kernel (Native) code and on top of existing and well tested
`Dict` type.

API mirrors the standard `Dict` (and [`Dict.Extra`](https://package.elm-lang.org/packages/elm-community/dict-extra/latest/Dict.Extra)) exactly where possible.

Some parts of the documentation are stolen directly from [`elm/core`](http://package.elm-lang.org/packages/elm/core/latest).
