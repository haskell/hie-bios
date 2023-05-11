# hie-bios

- [hie-bios](#hie-bios)
  - [Explicit Configuration](#explicit-configuration)
    - [Stack](#stack)
      - [Debugging a `stack` cradle](#debugging-a-stack-cradle)
      - [Ignoring directories](#ignoring-directories)
      - [Internal Libraries](#internal-libraries)
    - [Cabal](#cabal)
      - [Debugging a `cabal` cradle](#debugging-a-cabal-cradle)
      - [Ignoring directories](#ignoring-directories-1)
    - [Bios](#bios)
      - [Debugging a `bios` cradle](#debugging-a-bios-cradle)
    - [Direct](#direct)
      - [Debugging a `direct` cradle](#debugging-a-direct-cradle)
    - [None](#none)
  - [Multi-Cradle](#multi-cradle)
  - [Cradle Dependencies](#cradle-dependencies)
  - [Configuration specification](#configuration-specification)
  - [Testing your configuration](#testing-your-configuration)
  - [Implicit Configuration](#implicit-configuration)
    - [Priority](#priority)
    - [`cabal-install`](#cabal-install)
    - [`stack`](#stack-1)
    - [`bios`](#bios-1)
  - [Supporting Bazel and Obelisk](#supporting-bazel-and-obelisk)

`hie-bios` is the way to specify how
[`haskell-language-server`](https://github.com/haskell/haskell-language-server)
and [`ghcide`](https://github.com/digital-asset/ghcide) set up a GHC API session.

Given a Haskell project that is managed by Stack, Cabal, or other package tools,
`haskell-language-server` needs to know the full set of flags to pass to GHC in
order to build the project. These flags might contain some compilation options
like `-O2`, but a lot of the time they are package dependencies such as
`-package-id directory-1.3.6.0`, which also need to be built beforehand.
`hie-bios` satisfies both these needs.

Its design is motivated by the guiding principle:

> It is the responsibility of the build tool to describe the environment
> which a package should be built in.

Using this principle, it is possible
to easily support a wide range of tools including `cabal-install`, `stack`,
`rules_haskell`, `hadrian` and `obelisk` without major contortions.
`hie-bios` does not depend on the `Cabal` library nor does it
read any complicated build products and so on.

How does a tool specify a session? A session is fully specified by a set of
standard GHC flags. Most tools already produce this information if they support
a `repl` command. Launching a repl is achieved by calling `ghci` with the
right flags to specify the package database. `hie-bios` needs a way to get
these flags and then it can set up a GHC API session correctly.

Further it means that any failure to set up the API session is the responsibility
of the build tool. It is up to them to provide the correct information if they
want the tool to work correctly.

## Explicit Configuration

The user can place a `hie.yaml` file in the root of the workspace which
describes how to set up the environment. There are several supported ways to setup the environment.

### Stack

To explicitly state that you want to use `stack`, the basic configuration `hie.yaml` would look like:

```yaml
cradle:
  stack:
```

This configuration suffices if your whole project can be loaded by the command `stack repl`. As a rule of thumb, this works if the project consists of only one executable, one library and one test-suite.

Some projects have multiple `stack-*.yaml` files for multiple versions of ghc/resolvers. In this case you
can specify an alternate relative file to use by using the `stackYaml` option. The path is relative to the
configuration file.

```yaml
cradle:
  stack:
    stackYaml: "./stack-8.8.3.yaml"
```

If your project is more complicated, you need to specify which [components](https://docs.haskellstack.org/en/stable/build_command/#components) you want to load. A component is, roughly speaking, a library/executable/test-suite or benchmark in `stack`. You can view the components/targets of a stack project by executing the command:
``` sh
$ stack ide targets
```

Since we have two test-suites, one executable and a library, for `hie-bios`, this would output the following:

``` sh
$ stack ide targets
hie-bios:lib
hie-bios:exe:hie-bios
hie-bios:test:bios-tests
hie-bios:test:parser-tests
```

For an explanation of the target syntax, we refer to the documentation of the [target syntax](https://docs.haskellstack.org/en/stable/build_command/#target-syntax).

To tell `hie-bios` which component it should load, the following `hie.yaml` can be used:

```yaml
cradle:
  stack:
    component: "<component name>"
```

where `<component name>` is the name of the component/target you want to load.
While the component is optional, this is recommended to make sure the correct component is loaded.

Why is this not enough? Usually, you have multiple components with different dependencies. Your library won't depend on `tasty` or `hspec`, but your test-suite might. With this setup, you would only be able to load files from the given component.

Since you rarely only want to load a single component in a `stack` project, we have special syntax to be able to conveniently specify which directory belongs to which component. It is basically a [multi-cradle](#multi-cradle).

```yaml
cradle:
  stack:
    - path: "./src"
      component: "hie-bios:lib"
    - path: "./exe"
      component: "hie-bios:exe:hie-bios"
    - path: "./tests/BiosTests.hs"
      component: "hie-bios:test:hie-bios"
    - path: "./tests/ParserTests.hs"
      component: "hie-bios:test:parser-tests"
```

Here you can see two important features:
  * We provide a mapping from a filepath to component.
    * That way, we specify that a file such as `./src/HIE/Bios.hs` belongs to the component `hie-bios:lib`.
  * The filepath can be a file.
    * This is convenient if components are overlapping.

This way we specified which component needs to be compiled given a source file for our whole project.

If you use both, multiple components and an alternate `stack.yaml` file, there is a way to specify defaults
for the path-specific configurations.

```yaml
cradle:
  stack:
    stackYaml: "stack-8.3.3.yaml"
    components:
    - path: "./src"
      component: "hie-bios:lib"
    - path: "./exe"
      component: "hie-bios:exe:hie-bios"
    - path: "./tests/BiosTests.hs"
      component: "hie-bios:test:hie-bios"
    - path: "./tests/ParserTests.hs"
      component: "hie-bios:test:parser-tests"
```

A word of warning: Due to current restrictions in the language server, as mentioned in [this bug report](https://github.com/haskell/haskell-language-server/issues/268#issuecomment-667640809) all referenced stack.yaml files must specify the same version of GHC, as only one version of ghcide is loaded at a time per workspace root. This restriction might be lifted in the future.

#### Debugging a `stack` cradle

If you find that `hie-bios` can't load a certain component or file, run `stack repl` and `stack repl <component name>` to see if `stack` succeeds in building your project. Chances are that there is a problem in your project and if you fix that, `hie-bios` will succeed to load it.

Also, see notes for [testing your configuration](#testing-your-configuration).

Otherwise, please open an [issue](https://github.com/mpickering/hie-bios/issues/new).

#### Ignoring directories

You can combine the [multi-cradle](#multi-cradle) with a [none-cradle](#none-cradle) to ignore all source files in a certain directory. The syntax is a bit verbose:

```yaml
cradle:
  multi:
    - path: "./tests/projects"
      config:
        cradle:
          none:
    - path: "./"
      config:
        cradle:
          stack:
            - path: "./src"
              component: "hie-bios:lib"
            - path: "./exe"
              component: "hie-bios:exe:hie-bios"
            - path: "./tests/BiosTests.hs"
              component: "hie-bios:test:hie-bios"
            - path: "./tests/ParserTests.hs"
              component: "hie-bios:test:parser-tests"
```

This way, we specify that we do not want to load any files in our test project directories.

#### Internal Libraries

Internal libraries are not well supported in `stack`. Since the syntax `stack repl <internal library name>` [doesn't work](https://github.com/commercialhaskell/stack/issues/4564), `hie-bios` will generally not work with internal libraries using `stack`.

### Cabal

To use `cabal`, the basic explicit configuration looks similar to `stack`'s configuration.

```yaml
cradle:
  cabal:
```

The implication of this configuration is a bit different, though. Given a source file to load, we will use `cabal repl <filename>` to find the component of the given filepath.

This configuration should work in (almost) every standard project setup, since `cabal` finds the component associated to a given source file.
However, due to an unfortunate [bug](https://github.com/haskell/cabal/issues/6622), this fails on some files with `cabal` versions older than `3.4`.
So, to make your project loadable by older `cabal` versions, you can specify a component to load.
A [component](https://cabal.readthedocs.io/en/latest/nix-local-build.html?highlight=component#cabal-v2-build) is roughly speaking a library, executable, test-suite or benchmark in `cabal`.
The `hie.yaml` file looks like this:

```yaml
cradle:
  cabal:
    component: <component name>
```

This tells `hie-bios` that whichever source file it tries to load, this source file should be handled as if it belongs to `<component name>`.

As an example, to load the library of `hie-bios`, the following `hie.yaml` can be used:

```yaml
cradle:
  cabal:
    component: "lib:hie-bios"
```

The component syntax `"lib:hie-bios"` refers to the library of the package `hie-bios`. For a complete reference of the component syntax, we refer to the [documentation](https://cabal.readthedocs.io/en/latest/nix-local-build.html?highlight=component#cabal-v2-build).

Note that `cabal` and `stack` have different ways of specifying their
components.


If we only specify a single component, then we can only load source files from this component. This is unsatisfactory as we want to be able to navigate our project freely and work on multiple components (test-suite, library, executable, etc...) in parallel.

In a project such as `hie-bios`, we have more than one component, in particular we have four:

 * An executable
 * A library
 * Two test-suites

The component syntax can easily be extracted from the `hie-bios.cabal` file. Relevant sections are:

```cabal
...
Name:                   hie-bios
...

Library
  ...
  HS-Source-Dirs:       src

Executable hie-bios
  ...
  Main-Is:              Main.hs
  HS-Source-Dirs:       exe

test-suite parser-tests
  ...
  hs-source-dirs: tests/
  main-is: ParserTests.hs

test-suite bios-tests
  ...
  hs-source-dirs: tests/
  main-is: BiosTests.hs
```

Using the documentation of cabal, we extract the four component names of the `hie-bios` project:

* `lib:hie-bios`
* `exe:hie-bios`
* `test:bios-tests`
* `test:parser-tests`

Since you rarely only want to load a single component in a `cabal` project, we have special syntax to be able to conveniently specify which directory belongs to which component. It is basically a [multi-cradle](#multi-cradle).

```yaml
cradle:
  cabal:
    - path: "./src"
      component: "lib:hie-bios"
    - path: "./exe"
      component: "exe:hie-bios"
    - path: "./tests/BiosTests.hs"
      component: "test:hie-bios"
    - path: "./tests/ParserTests.hs"
      component: "test:parser-tests"
```

Here you can see two important features:
  * We provide a mapping from filepath to component.
    * That way, we specify that a file such as `./src/HIE/Bios.hs` belongs to the component `lib:hie-bios`.
  * The filepath can be a file.
    * This is convenient if components are overlapping.

Similarly to `multi-stack` configurations, you can also specify multiple components using a `components` subkey.

```yaml
cradle:
  cabal:
    components:
    - path: "./src"
      component: "lib:hie-bios"
    - path: "./exe"
      component: "exe:hie-bios"
    - path: "./tests/BiosTests.hs"
      component: "test:hie-bios"
    - path: "./tests/ParserTests.hs"
      component: "test:parser-tests"
```

This way we specified which component needs to be compiled given a certain source file for our whole project.

Some projects have multiple `cabal.project` files for multiple versions of ghc or development options. In this case you
can specify an alternate relative file to use by using the `project-file` option. The path is relative to the
configuration file.

```yaml
cradle:
  cabal:
    project-file: "./cabal.project.dev"
```

We can combine the `project-file` field with `components`:

```yaml
cradle:
  cabal:
    project-file: "./cabal.project.dev"
    components:
    - path: "./src"
      component: "lib:hie-bios"
    - path: "./exe"
      component: "exe:hie-bios"
    - path: "./tests/BiosTests.hs"
      component: "test:hie-bios"
    - path: "./tests/ParserTests.hs"
      component: "test:parser-tests"
```

#### Debugging a `cabal` cradle

If you find that `hie-bios` can't load a certain component or file, you may run `cabal repl <filename>` and `cabal repl <component name>` to see if `cabal` succeeds in building the components. Chances are that there is a problem and if you fix that, `hie-bios` will succeed to load the project.

Also, see notes for [testing your configuration](#testing-your-configuration).

Otherwise, please open an [issue](https://github.com/mpickering/hie-bios/issues/new).

#### Ignoring directories

You can combine the [multi-cradle](#multi-cradle) with a [none-cradle](#none-cradle) to ignore all source files in a certain directory. The syntax is a bit verbose:

```yaml
cradle:
  multi:
    - path: "./tests/projects"
      config:
        cradle:
          none:
    - path: "./"
      config:
        cradle:
          cabal:
            - path: "./src"
              component: "lib:hie-bios"
            - path: "./exe"
              component: "exe:hie-bios"
            - path: "./tests/BiosTests.hs"
              component: "test:hie-bios"
            - path: "./tests/ParserTests.hs"
              component: "test:parser-tests"
```

This way, we specify that we do not want to load any files in our test project directories.

### Bios

Alternatively you can explicitly state a `program` or shell command which should
be used to collect the options. This is the most general approach and can be extended to handle arbitrary build systems.

The path of the `program` attribute is interpreted relative to the current
working directory if it isn't absolute. A program is passed the file to
return options for as its first argument, and a shell command will have it
available in the `HIE_BIOS_ARG` environment variable.

There are two important environment variables:

* `HIE_BIOS_OUTPUT`: describes the filepath the options should be written to. If this file does not exist, the `program` should create it.
* `HIE_BIOS_ARG`: the source file that we want to load. Options returned by the `program` should be able to compile the given source file.
  * This environment variable is *only* available if a shell program is given.

The program flow is roughly as follows:
The process must consult the `HIE_BIOS_OUTPUT` environment variable and write a
list of options to this file, separated by newlines. Once the process finishes
running, `hie-bios` reads this file and uses the arguments to set up the GHC
session. This is how GHC's build system is able to support `hie-bios`.
Note, the `program` is intended to produce the build flags to compile the *whole* component the given source file belongs to. This entails that the `program` lists all of the component's module- and file targets.

A good guiding specification for this file is that the following commands
should work for any file in your project.

``` sh
$ export HIE_BIOS_OUTPUT=./options.txt # this is usually some temporary file
$ ./<program> /path/to/foo.hs
$ ghci $(cat $HIE_BIOS_OUTPUT | tr '\n' ' ')
```

where `HIE_BIOS_OUTPUT` is some chosen output file and `HIE_BIOS_ARG` contains the file parameter.

The `hie.yaml` configuration looks like this:

```yaml
cradle:
  bios:
    program: "<program>"
```

Alternatively, you may specify shell code directly.
This is helpful, if your `program` executable consists of only a single call to another executable.

```yaml
cradle:
  bios:
    shell: "<build-tool flags $HIE_BIOS_ARG>"
```

Additionally, you may specify the path to ghc. Otherwise, the one in the PATH will be used:

```yaml
cradle:
  bios:
    program: "<program>"
    with-ghc: "<ghc>"
```
#### Debugging a `bios` cradle

The most common error in creating `bios` cradle is to not list all targets of the component. Please make sure, that you always list all targets of the component, associated with the filepath you want to load.

Also, see notes for [testing your configuration](#testing-your-configuration).

### Direct

The `direct` cradle allows you to specify exactly the GHC options that should be used to load
a project. This is good for debugging but not a very good approach in general as the set of options
will quickly get out of sync with a cabal file.

```yaml
cradle:
  direct:
    arguments: [arg1, arg2]
```

#### Debugging a `direct` cradle

The arguments of a `direct` cradle will be passed almost directly to `ghc`. If the command `ghc <cradle arguments>` succeeds, then `hie-bios` can load the project.

### None

The `none` cradle says that the IDE shouldn't even try to load the project. It
is most useful when combined with the [multi-cradle](#multi-cradle) which is specified in the next section.

```yaml
cradle:
  none:
```

## Multi-Cradle

For a multi-component project you can use the multi-cradle to specify how each
subdirectory of the project should be handled by the IDE.

The multi-cradle is a list of relative paths and cradle configurations.
The path is relative to the configuration file and specifies the scope of
the cradle. For example, this configuration specifies that files in the
`src` subdirectory should be handled with the `lib:hie-bios` component and
files in the `test` directory using the `test:bios-tests` component.

```yaml
cradle:
  multi:
    - path: "./src"
      config:
        cradle:
          cabal:
            component: "lib:hie-bios"
    - path: "./test"
      config:
        cradle:
          cabal:
            component: "test:bios-tests"
```

If a file matches multiple prefixes, the most specific one is chosen.
Once a prefix is matched, the selected cradle is used to find the options. This
is usually a specific cradle such as `cabal` or `stack` but it could be another
multi-cradle, in which case, matching works in exactly the same way until a
specific cradle is chosen.

This cradle type is experimental and may not be supported correctly by
some libraries which use `hie-bios`. It requires some additional care to
correctly manage multiple components.

Note: Remember you can use the multi-cradle to declare that certain directories
shouldn't be loaded by an IDE, in conjunction with the `none` cradle.

```yaml
cradle:
  multi:
    - path: "./src"
      config: { cradle: {cabal: {component: "lib:hie-bios"}} }
    - path: "./test"
      config: { cradle: {cabal: {component: "test:bios-tests"}} }
    - path: "./test/test-files"
      config: { cradle: { none: } }
```

For cabal and stack projects there is a shorthand to specify how to load each component.

```yaml
cradle:
  cabal:
    - path: "./src"
      component: "lib:hie-bios"
    - path: "./test"
      component: "test:bios-tests"
```

```yaml
cradle:
  stack:
    - path: "./src"
      component: "hie-bios:lib"
    - path: "./test"
      component: "hie-bios:test:bios-tests"
```

Remember you can combine this shorthand with more complicated configurations
as well.

```yaml
cradle:
  multi:
    - path: "./test/testdata"
      config: { cradle: { none:  } }
    - path: "./"
      config: { cradle: { cabal:
                            [ { path: "./src", component: "lib:hie-bios" }
                            , { path: "./tests", component: "parser-tests" } ] } }
```

## Cradle Dependencies

Sometimes it is necessary to reload a component, for example when a package
dependency is added to the project. Each type of cradle defines a list of
files that might cause an existing cradle to no longer provide accurate
diagnostics if changed. These are expected to be relative to the root of
the cradle.

This makes it possible to watch for changes to these files and reload the
cradle appropiately.
However, if there are files that are not covered by
the cradle dependency resolution, you can add these files explicitly to
`hie.yaml`.
The file dependencies are not required to actually exist, since it can be useful
to know when they are created, e.g. if there was no `cabal.project`
in the project before and now there is, it might change how a file in the
project is compiled.

Here's an example of how you would add cradle dependencies that may not be covered
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

For the `Bios` cradle type, the newline-separated cradle dependencies must be written out
to the file specified by the `HIE_BIOS_DEPS` environment variable.

Previous versions implemented a different mechanism for collecting cradle dependencies
by means of a second program/shell field. This is still supported for backwards
compatibility:

```yaml
cradle:
  bios:
    dependency-program: ./dependency.sh
```
```yaml
cradle:
  bios:
    dependency-shell: build-tool dependencies $HIE_BIOS_ARG > $HIE_BIOS_OUTPUT
```

## Configuration specification

The complete configuration is a subset of

```yaml
cradle:
  cabal:
    component: "optional component name"
  stack:
    component: "optional component name"
  bios:
    program: "program to run"
    dependency-program: "optional program to run"
    shell: build-tool flags $HIE_BIOS_ARG
    dependency-shell: build-tool dependencies $HIE_BIOS_ARG
    with-ghc: "optional path to ghc"
  direct:
    arguments: ["list","of","ghc","arguments"]
  none:
  multi: - path: ./
           config: { cradle: ... }

dependencies:
  - someDep
```

## Testing your configuration

The given `hie-bios` executable is provided to test your configuration.

The `flags` command will print out the options that `hie-bios` thinks you will need to load a file.

``` sh
$ hie-bios flags exe/Main.hs
```

The `check` command will try to use these flags to load the module into the GHC API.

``` sh
$ hie-bios check exe/Main.hs
```

The `debug` command prints verbose information about the cradle, such as where the `hie.yaml` file was found, which file is loaded and the options that will eventually be used for loading a session.

``` sh
$ hie-bios debug exe/Main.hs
```

## Implicit Configuration

There are several built in modes which capture the most common Haskell development
scenarios. If no `hie.yaml` configuration file is found then an implicit
configuration is searched for. It is strongly recommended to just explicitly
configure your project.

### Priority

The targets are searched for in the following order.

1. A specific `.hie-bios` file.
2. A `stack` project
3. A `cabal` project
4. The direct cradle which has no specific options.

### `cabal-install`

The workspace root is the first folder containing a `cabal.project` file.

The arguments are collected by running `cabal v2-repl <filename>`.

If `cabal v2-repl <filename>` fails, then the user needs to configure the correct
target to use by writing a `hie.yaml` file.

### `stack`

The workspace root is the first folder containing a `stack.yaml` file.

The arguments are collected by executing `stack repl`. If this fails, the user needs to configure the correct target to use by writing a `hie.yaml` file.

### `bios`

The most general form is the `bios` mode which allows users to specify
which flags to provide themselves.

The program will receive the file to return options for as its first argument.

The program flow is roughly as follows:
The process must consult the `HIE_BIOS_OUTPUT` environment variable and write a
list of options to the file pointed to by `HIE_BIOS_OUTPUT`, separated by newlines. Once the process finishes running, `hie-bios` reads this file and uses the arguments to set up the GHC
session. This is how GHC's build system is able to support `hie-bios`.
Note, the `program` is intended to produce the build flags to compile the *whole* component the given source file belongs to. This entails that the `program` lists all of the component's module- and file targets.

A good guiding specification for this file is that the following commands
should work for any file in your project.
``` sh
$ export HIE_BIOS_OUTPUT=./options.txt # this is usually some temporary file
$ ./.hie-bios /path/to/foo.hs
$ ghci $(cat $HIE_BIOS_OUTPUT | tr '\n' ' ')
```

This is useful if you are designing a new build system or the other modes
fail to setup the correct session for some reason. For example, this is
how hadrian (GHC's build system) is integrated into `hie-bios`.

## Supporting Bazel and Obelisk

In previous versions of `hie-bios` there was also support for projects using `rules_haskell` and `obelisk`.
This was removed in the 0.3 release as they were unused and broken. There is no conceptual barrier to adding back support but it requires a user of these two approaches to maintain them.
