## Changelog for Weeder

### [`2.4.1`][v2.4.1] - *2023-01-05*

- Build with `lens-5.2`

### [`2.4.0`][v2.4.0] - *2022-05-22*

- Weeder has been upgraded to support GHC 9.2 (only). As this changes the
  format of `.hie` files accepted, this is a major version bump.

### [`2.3.1`][v2.3.1] - *2022-05-21*

This is the last release of `weeder` compatible with GHC 9.0.

- Weeder now analyzes top-level pattern bindings. For example, with the following input:

    ```haskell
    module Dep (a, b) where

    xxx :: Int
    xxx = 3

    a, b :: Int
    (a, b) = (xxx, 1)
    ```

    ... `weeder` will determine that both `a` and `b` depend on `xxx`. While this is an over-approximation, it prevents weeder from reporting false positives. For more information, see [#92](https://github.com/ocharles/weeder/issues/92).

- Corrected a typo in `--help` ([#96](https://github.com/ocharles/weeder/pull/96)).
- Shorten the help text for `--require-hs-files` ([#97](https://github.com/ocharles/weeder/pull/97)).
- Allow `algebraic-graphs-0.6` ([#95](https://github.com/ocharles/weeder/pull/95)).
- Allow Dhall 1.41 ([#99](https://github.com/ocharles/weeder/pull/99)).
- Allow `optparse-applicative-0.17` ([#100](https://github.com/ocharles/weeder/pull/100)).

### [`2.3.0`][v2.3.0] - *2021-10-11*

- Switch to GHC 9

### [`2.2.0`][v2.2.0] - *2021-08-28*

This will likely be the last Weeder release before GHC 9.

- Allow configuration of the HIE file extension using the `--hie-extension` command-line flag
- Add `--require-hs-files` switch. If supplied, this switch means Weeder will only consider `.hie` files where a corresponding `.hs` file can be found. ([#50](https://github.com/ocharles/weeder/pull/50))
- Pattern synonyms are now considered ([#79](https://github.com/ocharles/weeder/pull/79))
- Weeder's output format is now one-line-per-weed ([#62](https://github.com/ocharles/weeder/pull/62))
- `--hie-extension` can be used to change the extension used for `.hie` files ([#64](https://github.com/ocharles/weeder/pull/64))

### [`2.1.3`][v2.1.3] - *2020-12-11*

- Support `dhall-1.35`, `dhall-1.36` and `dhall-1.37`.
- Support `bytestring-0.11`.

### [`2.1.2`][v2.1.2] - *2020-09-09*

- Correctly support optparse-applicative 0.16.0.0. While this was meant to be resolved 2.1.1, 2.1.1 only changed the version for the library and not the executable.

### [`2.1.1`][v2.1.1] - *2020-09-09*

- Support Dhall 1.34
- Support optparse-applicative 0.16.0.0

### [`2.1.0`][v2.1.0] - *2020-06-30*

- Support regex-tdfa ^>= 1.2 (#13)
- Handle mismatched hie file version explicitly (fixes #8). (#16)
- Implement --help and --version (#36)
- Make search directory configurable (#37)
- Support GHC 8.10

### [`2.0.1`][v2.0.1] - *2020-03-15*

- PR #7: Support optparse-applicative-0.15.1.0 and algebraic-graphs-0.5. Thanks
  to @robx for contribuiting this fix!


### [`2.0.0`][v2.0.0] - *2020-03-15*

- Weeder 2.0 is a ground up rewrite of Weeder using `.hie` files. It is now
maintained by Ollie Charles (@ocharles on GitHub).

- **Note:**

  Issue numbers *before* this version reference to the original repository here:

    - https://github.com/ndmitchell/weeder

  Issue numbers *after*  this version reference to the new repository here:

    - https://github.com/ocharles/weeder


### [`1.0.8`][v1.0.8] - *2018-08-26*

- #42, make paths case-insensitive on MacOS


### [`1.0.7`][v1.0.7] - *2018-08-23*

- Don't warn on base as it is used by Paths_ modules

- #42, make --verbose print out the version number

- #41, make the --help output clear you can pass a stack.yaml


### [`1.0.6`][v1.0.6] - *2018-06-16*

- Don't fail with an error if stack setup is necessary

- If you fail to find stack.yaml give a better error message


### [`1.0.5`][v1.0.5] - *2018-05-05*

- #39, provide weeder as a library


### [`1.0.4`][v1.0.4] - *2018-05-02*

- #38, make sure you parse bracketed version ranges properly


### [`1.0.3`][v1.0.3] - *2018-03-04*

- #35, support ^>= operator in Cabal


### [`1.0.2`][v1.0.2] - *2018-03-01*

- Add lower bounds for Yaml and Aeson


### [`1.0.1`][v1.0.1] - *2018-02-23*

- #34, support -any for version numbers


### [`1.0`][v1.0] - *2018-01-22*

- #30, bump the version number to 1.0


### [`0.1.13`][v0.1.13] - *2018-01-17*

- #32, find .hi files in more places

- #32, always disable color when running stack


### [`0.1.12`][v0.1.12] - *2018-01-16*

- Make available on Mac


### [`0.1.11`][v0.1.11] - *2017-12-29

- #29, deal with case-insensitive FilePath on Windows


### [`0.1.10`][v0.1.10] - *2017-12-28*

- Make --verbose print out the directory when running commands

- Don't report semigroups as unused on any platforms


### [`0.1.9`][v0.1.9] - *2017-12-07*

- Don't report Win32/unix as unused on the alternate platform


### [`0.1.8`][v0.1.8] - *2017-12-06*

- Follow both branches for if/else containing dependencies/modules


### [`0.1.7`][v0.1.7] - *2017-08-09*

- #21, detect dependencies that are only required transitively

- #13, respect the STACK_YAML environment variable

- #20, add verbosity messages in a lot of places

- #15, tone down unused import if exporting a cross-package type

- #11, optimise execution speed (~3x faster)


### [`0.1.6`][v0.1.6] - *2017-06-18*

- #10, find files generated by alex/happy


### [`0.1.5`][v0.1.5] - *2017-06-02*

- If --yaml and no hints give no output


### [`0.1.4`][v0.1.4] - *2017-05-27*

- #9, allow --dist-dir to set the stack dist-dir

- Deal with operators including | in them

- Allow arrays of arrays of strings in the .weeder.yaml


### [`0.1.3`][v0.1.3] - *2017-05-08*

- #5, document how to install weeder

- #8, detect unused imports, even import Foo()

- #7, don't say modules with only instances are always redundant

- #6, don't give partial pattern matches when reading .weeder.yaml


### [`0.1.2`][v0.1.2] - *2017-04-29*

- #3, deal with space-separated hs-source-dirs


### [`0.1.1`][v0.1.1] - *2017-04-29*

- #2, use "stack query" rather than parsing stack.yaml


### [`0.1`][v0.1] - *2017-04-28*

- Initial version


[v2.4.1]: https://github.com/ocharles/weeder/releases/tag/2.4.1
[v2.4.0]: https://github.com/ocharles/weeder/releases/tag/2.4.0
[v2.3.1]: https://github.com/ocharles/weeder/releases/tag/2.3.1
[v2.3.0 ]: https://github.com/ocharles/weeder/releases/tag/2.3.0
[v2.2.0 ]: https://github.com/ocharles/weeder/releases/tag/2.2.0
[v2.1.3 ]: https://github.com/ocharles/weeder/releases/tag/2.1.3
[v2.1.2 ]: https://github.com/ocharles/weeder/releases/tag/2.1.2
[v2.1.1 ]: https://github.com/ocharles/weeder/releases/tag/2.1.1
[v2.1.0 ]: https://github.com/ocharles/weeder/releases/tag/2.1.0
[v2.0.1 ]: https://github.com/ocharles/weeder/releases/tag/2.0.1
[v2.0.0 ]: https://github.com/ocharles/weeder/releases/tag/2.0.0
[v1.0.8 ]: https://github.com/ndmitchell/weeder/tree/v1.0.8
[v1.0.7 ]: https://github.com/ndmitchell/weeder/tree/v1.0.7
[v1.0.6 ]: https://github.com/ndmitchell/weeder/tree/v1.0.6
[v1.0.5 ]: https://github.com/ndmitchell/weeder/tree/v1.0.5
[v1.0.4 ]: https://github.com/ndmitchell/weeder/tree/v1.0.4
[v1.0.3 ]: https://github.com/ndmitchell/weeder/tree/v1.0.3
[v1.0.2 ]: https://github.com/ndmitchell/weeder/tree/v1.0.2
[v1.0.1 ]: https://github.com/ndmitchell/weeder/tree/v1.0.1
[v1.0   ]: https://github.com/ndmitchell/weeder/tree/v1.0
[v0.1.13]: https://github.com/ndmitchell/weeder/tree/v0.1.13
[v0.1.12]: https://github.com/ndmitchell/weeder/tree/v0.1.12
[v0.1.11]: https://github.com/ndmitchell/weeder/tree/v0.1.11
[v0.1.10]: https://github.com/ndmitchell/weeder/tree/v0.1.10
[v0.1.9 ]: https://github.com/ndmitchell/weeder/tree/v0.1.9
[v0.1.8 ]: https://github.com/ndmitchell/weeder/tree/v0.1.8
[v0.1.7 ]: https://github.com/ndmitchell/weeder/tree/v0.1.7
[v0.1.6 ]: https://github.com/ndmitchell/weeder/tree/v0.1.6
[v0.1.5 ]: https://github.com/ndmitchell/weeder/tree/v0.1.5
[v0.1.4 ]: https://github.com/ndmitchell/weeder/tree/v0.1.4
[v0.1.3 ]: https://github.com/ndmitchell/weeder/tree/v0.1.3
[v0.1.2 ]: https://github.com/ndmitchell/weeder/tree/v0.1.2
[v0.1.1 ]: https://github.com/ndmitchell/weeder/tree/v0.1.1
[v0.1   ]: https://github.com/ndmitchell/weeder/tree/v0.1
