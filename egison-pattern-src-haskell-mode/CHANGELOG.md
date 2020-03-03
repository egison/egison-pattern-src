## 0.2.0.0

Breaking changes:
- Add `ParseMode` record to pass parser configurations (change in [#19](https://github.com/egison/egison-pattern-src/issues/19))
- Provide several variants of parsers (location annotation/non-greedy) by adding `Parsable` instance (change in [#19](https://github.com/egison/egison-pattern-src/issues/19))
- Use canonical way to print infix operators [#20](https://github.com/egison/egison-pattern-src/issues/19)
- Some changes in `egison-pattern-src` are inherited here. For details, see [CHANGELOG](https://hackage.haskell.org/package/egison-pattern-src-0.2.0.0/changelog) in [egison-pattern-src-0.2.0.0](https://hackage.haskell.org/package/egison-pattern-src-0.2.0.0).

## 0.1.1.0

- Correct some documentations
- Add `parseExprWithParseFixities` to configure parsers flexibly (e.g. parse as `++`, but interpret it as `join`)

## 0.1.0.0

- Initial Release
