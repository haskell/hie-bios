# hie-bios

`hie-bios` is the way which
[`hie`](https://github.com/haskell/haskell-ide-engine) sets up a GHC API session.

Given a Haskell project that is managed by Stack, Cabal, or other package tools,
`hie` needs to know the full set of flags to pass to GHC in order to build the
project. `hie-bios` satisfies this need.

Its design is motivated by the guiding principle:

> It is the responsibility of the build tool to describe the environment
> which a package should be built in.

Using this principle, it is possible
to easily support a wide range of tools including `cabal-install`, `stack`,
`rules_haskell`, `hadrian` and `obelisk` without major contortions.
`hie-bios` does not depend on the `Cabal` library nor does not
read any complicated build products and so on.

How does a tool specify a session? A session is fully specified by a set of
standard GHC flags. Most tools already produce this information if they support
a `repl` command. Launching a repl is achieved by calling `ghci` with the
right flags to specify the package database. `hie-bios` needs a way to get
these flags and then it can set up GHC API session correctly.

Futher it means that any failure to set up the API session is the responsibility
of the build tool. It is up to them to provide the correct information if they
want `hie` to work correctly.

## Explicit Configuration

The user can place a `hie.yaml` file in the root of the workspace which
describes how to setup the environment. For example, to explicitly state
that you want to use `stack` then the configuration file would look like:

```yaml
cradle: {stack}
```

If you use `cabal` then you probably need to specify which component you want
to use.

```yaml
cradle: {cabal: {component: "lib:haskell-ide-engine"}}
```

Or you can explicitly state the program which should be used to collect
the options by supplying the path to the program. It is interpreted
relative to the current working directory if it is not an absolute path.

```yaml
cradle: {bios: {program: ".hie-bios"}}
```

### Cradle Dependencies

Sometimes it is necessary to reload a cradle, for example when a package
dependency is added to the project. Each type of cradle defines a list of
files that might cause an existing cradle to no longer provide accurate
diagnostics if changed. These are expected to be relative to the root of
the cradle.

This makes it possible to watch for changes to these files and reload the
cradle appropiately.
However, if there are files that are not covered by
the cradle dependency resolution, you can add these files explicitly to
`hie.yaml`.
These files are not required to actually exist, since it can be useful
to know when these files are created, e.g. if there was no `cabal.project`
in the project before and now there is, it might change how a file in the
project is compiled.

As an example how you would add cradle dependencies that may not be covered
by the `cabal` cradle.

```yaml
cradle:
  cabal:
    component: "lib:hie-bios"

dependencies:
  - package.yaml
  - shell.nix
  - default.nix
```

For the `Bios` cradle type, there is an optional field to specify a program
to obtain cradle dependencies from:

```yaml
cradle:
  bios:
    program: ./flags.sg
    dependency-program: ./dependency.sh
```

The program `./dependency.sh` is executed with no paramaters and it is
expected to output on stdout on each line exactly one filepath relative
to the root of the cradle, not relative to the location of the program.

## Configuration specification

The complete configuration is a subset of

```yaml
cradle:
  cabal:
    component: "optional component name"
  stack:
  bazel:
  obelisk:
  bios:
    program: "program to run"
    dependency-program: "optional program to run"
  direct:
    arguments: ["list","of","ghc","arguments"]
  default:
  none:
  multi: - path: ./
           config: { cradle: ... }

dependencies:
  - someDep
```

## Multi-Cradle

For a multi-component project you can use the multi-cradle to specify how each
subdirectory of the project should be handled by the IDE.

The multi-cradle is a list of relative paths and cradle configurations.
The path is relative to the configuration file and specifies the scope of
the cradle. For example, this configuration specificies that files in the
`src` subdirectory should be handled with the `lib:hie-bios` component and
files in the `test` directory using the `test` component.

```
cradle:
  multi:
    - path: "./src"
      config: { cradle: {cabal: {component: "lib:hie-bios"}} }
    - path: "./test"
      config: { cradle: {cabal: {component: "test"}} }
```

If a file matches multiple prefixes, the most specific one is chosen.

This cradle type is experimental and may not be supported correctly by
some libraries which use `hie-bios`. It requires some additional care to
correctly manage multiple components.

Note: Remember you can use the multi-cradle to declare that certain directories
shouldn't be loaded by an IDE, in conjunction with the `none` cradle.

```
cradle:
  multi:
    - path: "./src"
      config: { cradle: {cabal: {component: "lib:hie-bios"}} }
    - path: "./test"
      config: { cradle: {cabal: {component: "test"}} }
    - path: "./test/test-files"
      config: { cradle: none }
```


## Implicit Configuration

There are several built in modes which captures most common Haskell development
scenarios. If no `hie.yaml` configuration file is found then an implicit
configuration is searched for.

### Priority

The targets are searched for in following order.

1. A specific `hie-bios` file.
2. An `obelisk` project
3. A `rules_haskell` project
4. A `stack` project
5. A `cabal` project
6. The default cradle which has no specific options.

### `cabal-install`

The workspace root is the first folder containing a `cabal.project` file.

The arguments are collected by running `cabal v2-repl`.

If `cabal v2-repl` fails, then the user needs to configure the correct
target to use by writing a `hie.yaml` file.

### `rules_haskell`

The workspace root is the folder containing a `WORKSPACE` file.

The options are collected by querying `bazel`.

### `obelisk`

The workspace root is the folder containing a `.obelisk` directory.

The options are collected by running `ob ide-args`.

### `bios`

The most general form is the `bios` mode which allows a user to specify themselves
which flags to provide.

In this mode, an executable file called `.hie-bios` is placed in the root
of the workspace directory. The script takes one argument, the filepath
to the current file we want to load into the session. The script returns
the correct arguments in order to load that file successfully. The arguments
must be delimited by newline (`\n`); i.e., each argument must be in its own
line.

A good guiding specification for this file is that the following command
should work for any file in your project.

```
ghci $(./hie-bios /path/to/foo.hs) /path/to/foo.hs
```

This is useful if you are designing a new build system or the other modes
fail to setup the correct session for some reason. For example, this is
how hadrian (GHC's build system) is integrated into HIE.


## Relationship with `ghcid`

The design of `hie-bios` is inspired by `ghcid`. Like `ghcid`, it does not depend
on any of the tools it supports. The success of `ghcid` is that it works reliably
in many situations. This is because of the fact that it delegates complicated
decisions about a build to the build tool.

`ghcid` could be implemented using `hie-bios` using the `ghci $(./hie-bios Main.hs) Main.hs`
idiom described earlier.
