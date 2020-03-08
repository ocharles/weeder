# Weeder

Weeder is an application to perform whole-program dead-code analysis. Dead code
is code that is written, but never reachable from any other code. Over the
lifetime of a project, this happens as code is added and removed, and leftover
code is never cleaned up. While GHC has warnings to detect dead code is a single
module, these warnings don't extend across module boundaries - this is where
Weeder comes in.

Weeder uses HIE files produced by GHC - these files can be thought of as source
code that has been enhanced by GHC, adding full symbol resolution and type
information. Weeder builds a dependency graph from these files to understand how
code interacts. Once all analysis is done, Weeder performs a traversal of this
graph from a set of roots (e.g., your `main` function), and determines which
code is reachable and which code is dead.

# Using Weeder

## Preparing Your Code for Weeder

To use Weeder, you will need to generate `.hie` files from your source code. If
you use Cabal, this is easily done by adding one line to your
`cabal.project.local` file:

``` cabal
package *
  ghc-options: -fwrite-ide-info
```

Once this has been added, perform a full rebuild of your project:

``` shell
cabal clean
cabal build all
```

## Calling Weeder

When you call Weeder, you need to supply the directory containing `.hie` files -
`.` is ussually sufficient, and at least one root - the starting point for
alive-code analysis. If you're building an executable, the `main` function is a
good starting point:

``` shell
weeder . --root 'main Main main'
```

You can also supply additional roots by supply `--root` multiple times. The
syntax of the argument given to `--root` is either:

* `unit-id ModuleName symbolName`, where `symbolName` is the name of a variable
  (not a type).
* `unit-id ModuleName`. This form will add all exported symbols from the given
  module. This can be useful if you are writing a library.

# Limitations

Weeder currently has a few limitations:

## Type Class Instances

Weeder is not currently able to analyse whether a type class instance is used.
For this reason, Weeder adds all symbols referenced to from a type class
instance to the root set, keeping this code alive. In short, this means Weeder
might not detect dead code if it's used from a type class instance which is
never actually needed.

## Template Haskell

Weeder is currently unable to parse the result of a Template Haskell splice. If
some Template Haskell code refers to other source code, this dependency won't be
tracked by Weeder, and thus Weeder might end up with false positives.
