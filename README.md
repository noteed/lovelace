# Lovelace

See the top of `Lovelace.hs` for design notes.


## Building

```
$ nix-build release.nix
```


## Features

- When running a workflow, it is possible to get a trace of all the
  intermediate results by using the `runs` function (instead of the `run`
  function which returns only the final result).

- A simple SVG rendering is provided by using Graphviz. Use the `Makefile` to
  get an example:

```
$ make
```


## Developer notes

A `shell.nix` is provided and can be used as follow:

```
$ nix-shell
$ ghci bin/lovelace.hs
# Or directly:
$ nix-shell --pure --run 'ghci bin/lovelace.hs'
```

`default.nix` is generated from the `.cabal` file:

```
$ cabal2nix . > default.nix
```

A first test has been added and can be run with:

```
$ nix-shell --pure --run 'runghc -itests/ tests/bin/test-suite.hs'
```
