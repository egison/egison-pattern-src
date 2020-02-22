# Egison Pattern Source

[![Actions Status](https://github.com/egison/egison-pattern-src/workflows/latest/badge.svg)](https://github.com/egison/egison-pattern-src/actions?workflow=latest)
[![Hackage](https://img.shields.io/hackage/v/egison-pattern-src.svg)](https://hackage.haskell.org/package/egison-pattern-src)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/egison-pattern-src.svg)](http://packdeps.haskellers.com/reverse/egison-pattern-src)

The [egison-pattern-src](https://hackage.haskell.org/package/egison-pattern-src) provides a standalone syntax definition for patterns in [Egison programming language](https://egison.org/).
This package enables you to embed Egison's patterns in your parser (pretty-printer) by supplying expression and name parsers (printers) externally.
For the uses in Haskell source code, such adaptors for [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts) and [template-haskell](https://hackage.haskell.org/package/template-haskell) are also available here as [egison-pattern-src-haskell-mode](https://hackage.haskell.org/package/egison-pattern-src-haskell-mode) and [egison-pattern-src-th-mode](https://hackage.haskell.org/package/egison-pattern-src-th-mode) respectively.

## Syntax

The following is a simplified syntax of pattern expressions where `x`, `v`, `op` and `e` are respectively names, variable names, user-defined infix operators and expressions in host language.

```
p ::= _                     (wildcard pattern)
    | $v                    (pattern variable)
    | #e                    (value pattern)
    | ?e                    (predicate pattern)
    | p & p                 (and pattern)
    | p | p                 (or pattern)
    | !p                    (not pattern)
    | p op p                (user-defined infix pattern)
    | x                     (constructor pattern without argument)
    | (x p_1 p_2 ... p_n)   (constructor pattern with arguments)
```

## Development

To build and test `egison-pattern-src` locally, run:

```
$ make test    # or just `cabal new-test all`
```

You can use `make watch` to watch for file changes and re-run tests.

## License

[egison-pattern-src](https://hackage.haskell.org/package/egison-pattern-src) is distributed as open source software under the terms of the 3 clause BSD License. See [LICENSE](/LICENSE) for details.
