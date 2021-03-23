# ChangeLog hie-bios

## TBA - 0.7.6

* Log stderr of stack to display more informative error messages to users. [#254](https://github.com/mpickering/hie-bios/pull/254)
## 2021-03-21 - 0.7.5

### Bug Fixes

* Improve out-of-the-box support for dynamically linked GHC. [#286](https://github.com/mpickering/hie-bios/pull/286), [#287](https://github.com/mpickering/hie-bios/pull/287)

## 2021-02-19 - 0.7.4

### Bug Fixes

* Create the cache directory on linux if it is missing [#283](https://github.com/mpickering/hie-bios/pull/283)

## 2021-01-29 - 0.7.3

* Set builddir for cabal [#264](https://github.com/mpickering/hie-bios/pull/264)
  * Essentially, change the build directory for cabal to the [`XDG_CACHE_HOME`](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    directory (e.g. `~/.cache/hie-bios/...`). This way, user
    invocations of cabal will no longer trigger a `configure` step, improving
    the overall developer experience.
* Optparse-applicative CLI [#276](https://github.com/mpickering/hie-bios/pull/276)

## 2020-12-16 - 0.7.2

* Faster Bios protocol [#271](https://github.com/mpickering/hie-bios/pull/271)
* Modify unreachable cabal website links [#259](https://github.com/mpickering/hie-bios/pull/259)
* Only take the last line of output in getRuntimeGhcX [#256](https://github.com/mpickering/hie-bios/pull/256)

## 2020-09-01 - 0.7.1

* Add explicit type for stack.yaml location [#243](https://github.com/mpickering/hie-bios/pull/243)
  * In particular, fixes a regression with `hie.yaml` and standalone-files for stack
* Reduce noise in Extra-Source-File field [#239](https://github.com/mpickering/hie-bios/pull/239)

## 2020-08-27 - 0.7.0

### New Features

* Allow specifying a stack.yaml for stack configurations [#230](https://github.com/mpickering/hie-bios/pull/230)
* Pass HIE_BIOS_ARG to the dependencies program [#235](https://github.com/mpickering/hie-bios/pull/235)

### API Changes

* Change Config CradleType

## 2020-08-08 - 0.6.3

### API Addition

* Expose yamlConfig [#237](https://github.com/mpickering/hie-bios/pull/237)

## 2020-08-08 - 0.6.2

### New Features

* Add optional ghc-path field in bios cradles [#231](https://github.com/mpickering/hie-bios/pull/231)

## 2020-07-12 - 0.6.1

### Bug Fixes

* Expose 'readProcessWithCwd' [#227](https://github.com/mpickering/hie-bios/pull/227)
* Fix mistakes in the ChangeLog [#228](https://github.com/mpickering/hie-bios/pull/228)

## 2020-07-12 - 0.6.0

### New Features

* Add getRuntimeGhcLibDir and getRuntimeGhcVersion functions through a new runGhcCmd API [#207](https://github.com/mpickering/hie-bios/pull/207) [#224](https://github.com/mpickering/hie-bios/pull/224)
* Add shell and dependency-shell attributes to bios cradle type [#188](https://github.com/mpickering/hie-bios/pull/188)
* Store dependencies in CradleError [#186](https://github.com/mpickering/hie-bios/pull/186)

### Bug Fixes

* Improve the README [#225](https://github.com/mpickering/hie-bios/pull/225)
* Detect implicit cabal cradle in the absence of cabal.project [#221](https://github.com/mpickering/hie-bios/pull/221)
* Dont resolve symlinks in cradle discovery [#219](https://github.com/mpickering/hie-bios/pull/219)
* Make Cradle dependencies for stack and cabal more reasonable [#209](https://github.com/mpickering/hie-bios/pull/209)
  * This ships with a known bug: `stack` lists cradle dependencies from
	sub-directories incorrectly.
* Fix absolute mains [#205](https://github.com/mpickering/hie-bios/pull/205)
* Improve filtering of rts arguments from stack and cabal cradles [#197](https://github.com/mpickering/hie-bios/pull/197)
* Make package db paths absolute [#193](https://github.com/mpickering/hie-bios/pull/193)
* Add cabal.project.local to cabal cradle dependencies [#184](https://github.com/mpickering/hie-bios/pull/184)
* Remove outdated reference to $HIE_BIOS_GHC[_ARGS]

## 2020-06-26 - 0.5.1

* Fix printing of current directory in wrapper script [#206](https://github.com/mpickering/hie-bios/pull/206)
* Export Cradle utilizes [#189](https://github.com/mpickering/hie-bios/pull/189)

## 2020-05-08 - 0.5.0

* Add cabal.project.local to cabal cradle dependencies [#184](https://github.com/mpickering/hie-bios/pull/184)
* Remove unused environment variables to simplify code. [#182](https://github.com/mpickering/hie-bios/pull/182)
* Clean up hie-bios wrapper scripts after they are used. [#179](https://github.com/mpickering/hie-bios/pull/179)
* Avoid error in windows due to temp file being locked. [#175](https://github.com/mpickering/hie-bios/pull/175)
* Get building with ghc-8.10. [#173](https://github.com/mpickering/hie-bios/pull/173)
* Add getCompilerOptionsWithLogger convenience function.
* Add componentRoot to ComponentOptions. [#166](https://github.com/mpickering/hie-bios/pull/166)
Options may be relative to the componentRoot.
* Add makeDynFlagsAbsolute to fix mangling of ghc options starting with "-i". [#166](https://github.com/mpickering/hie-bios/pull/166)
Breaks backwards-compatibility, because ComponentOptions now may contain
filepaths relative to the component root directory.
This function needs to be invoked on the parsed 'DynFlags' to normalise the filepaths.
* Fix Ghci Script parses space in Filepath as Module (#162)
* Correct path to .hie-bios example in readme (#159)
* Relax upper bound for 'extra' (#161)

## 2020-01-29 - 0.4.0

* Return CompilerOptions in initialization (#130)
* Implement hook into config parser (#131)
* Enable GHC 8.8.1 windows ci (#128)
* Catch permission errors in cradle discovery (#127)
* Add explicit cradle predicates and multi cradle depend on its cradles (#119)
* Fix outdated direct cradle in README (#124)
* Pass filepath to cabal v2-repl when getting flags (#123)
* CPP for GHC 8.10 compatibility (#134)
* Derive Ord for ComponentOptions (#133)
* Lower the required version of the GHC dependency (#138)
* Add tests for implicit cradles (#135)
* Add Functor instance for Cradle and ActionName (#140)
* Remove Show instance from public API (#146)
* Add Show instance for CradleLoadResult (#145)
* Typo in debug message (#144)
* Add lower bound for aeson and clean-up API (#142)

## 2019-12-19 - 0.3.2

* Compile windows wrapper script in a a more appropiate directory. (#109)
* Fix situation in wrapper script when environmental variable wasn't set. (#109)

## 2019-12-18 - 0.3.1

* Fix bug in the windows wrapper script (#108)

## 2019-12-15 - 0.3.0

* Add multi cradle, cabal multi cradle and none cradle
* Remove obelisk, bazel and default cradle types
* bios program now expects arguments to be separated by newlines rather than
spaces. (#80)
* Only try to use stack cradle if `stack` is executable.
* Filter out `-w -v0` from cabal output when using cabal cradle.
* Initialise plugins when loading a module.
* Interface file cache persists between loads -- this greatly speeds up
reloading a project if the options don't change.
* Reuse wrapper executable on windows if one already exists.
* Make stack cradle work more like the cabal cradle
- Syntax for specifying a specific component
- Targets are read from the ghci script file
* Cradles now use a temporary file to communicate arguments to hie-bios.
bios cradles should consult the HIE_BIOS_OUTPUT envvar for the filepath to
write the arguments seperated by newlines.

## 2019-09-19 - 0.2.1

* Make stack cradle use the same wrappers as cabal cradle. Fixes some issues
on windows.

## 2019-09-18 - 0.2.0

* Compat with 8.2 and 8.8
* Add support for explicitly specifying dependencies for a cradle
* Separate arguments by null bytes, so arguments can contain spaces
(cabal/stack wrapper)
* Add --help to CLI
* Fix the directories that certain processes run in

## 2019-09-07 - 0.1.1

* Compat with GHC 8.4
* Fix long paths issue on windows
* Handle projects with .o files

## 2019-09-06 - 0.1.0

* First release
