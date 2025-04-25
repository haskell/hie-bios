# ChangeLog hie-bios

## 2025-04-25 - 0.15.0

* Use consistent logging for test cases [#461](https://github.com/haskell/hie-bios/pull/461)
* Use cabal path when available [#458](https://github.com/haskell/hie-bios/pull/458)
* Keep track of not loaded files for cabal [#453](https://github.com/haskell/hie-bios/pull/453)
* Allow GHC 9.12 [#449](https://github.com/haskell/hie-bios/pull/449)
* Fix cabal check [#448](https://github.com/haskell/hie-bios/pull/448)

## 2024-05-20 - 0.14.1

* Allow building with GHC 9.10.1 [#435](https://github.com/haskell/hie-bios/pull/435)

## 2024-04-22 - 0.14.0

* Add Loading Style option to 'runAction' [#433](https://github.com/haskell/hie-bios/pull/433)
* Cleanup CPP for GHCs < 9.2, fix most GHC warnings [#429](https://github.com/haskell/hie-bios/pull/429)
* Update GHC versions in CI, drop ghcs not supported by hls [#428](https://github.com/haskell/hie-bios/pull/428)

## 2023-11-14 - 0.13.1

* Add CI support for GHC 9.8.1 [#419](https://github.com/haskell/hie-bios/pull/419)
* 9.8 support [#417](https://github.com/haskell/hie-bios/pull/417)
* Avoid deadlocks in multi-component support [#416](https://github.com/haskell/hie-bios/pull/416)
* Accept directories in 'findCradle' [#415](https://github.com/haskell/hie-bios/pull/415)
* Drop old GHC version support [#414](https://github.com/haskell/hie-bios/pull/414)

## 2023-08-22 - 0.13.0

* Multi Component cabal support [#409](https://github.com/haskell/hie-bios/pull/409)
* Make sure cabal caches can be found [#408](https://github.com/haskell/hie-bios/pull/408)
* Rename project-file to cabalProject in hie.yaml [#407](https://github.com/haskell/hie-bios/pull/407)
* Update README for new project-file key [#403](https://github.com/haskell/hie-bios/pull/403)
* Add more informative log messages for cradle running [#406](https://github.com/haskell/hie-bios/pull/406)
* Add cabal.project support for cabal cradles [#357](https://github.com/haskell/hie-bios/pull/357)

## 2023-11-13 - 0.12.1

* 9.8 support [#417](https://github.com/haskell/hie-bios/pull/417)

## 2023-03-13 - 0.12.0

* 9.6 support [#392](https://github.com/haskell/hie-bios/pull/392)
* Better support for multi component projects [#387](https://github.com/haskell/hie-bios/pull/387)
* Remove unused dependencies from hie-bios [#381](https://github.com/haskell/hie-bios/pull/381)
* Add logs over commands [#375](https://github.com/haskell/hie-bios/pull/375)

## 2022-09-13 - 0.11.0

* Compatibility with aeson 1.5 [#368](https://github.com/haskell/hie-bios/pull/368)
* Add GHC 9.4 support [#366](https://github.com/haskell/hie-bios/pull/366)
* Actually run the bios-tests when tool-deps are ignored [#365](https://github.com/haskell/hie-bios/pull/365)
  * They have been accidentally disabled since 0.9.0.
* Completely overhaul test-suite [#356](https://github.com/haskell/hie-bios/pull/356)

## 2022-07-26 - 0.10.0

* Apply Hlint suggestions [#354](https://github.com/haskell/hie-bios/pull/354)
* Cabal cradle: change error message on failure [#353](https://github.com/haskell/hie-bios/pull/353)
* Refactor parsing of hie.yaml files [#329](https://github.com/haskell/hie-bios/pull/329)
* Make sure we test the same versions as HLS [#346](https://github.com/haskell/hie-bios/pull/346)
* Move logging from hslogger to co-log  [#347](https://github.com/haskell/hie-bios/pull/347)
  * Demote process output to Debug severity [#348](https://github.com/haskell/hie-bios/pull/348)
* Fix typos [#342](https://github.com/haskell/hie-bios/pull/342)

## 2022-03-07 - 0.9.1

* Ignore .ghci files while querying project GHC [#337](https://github.com/haskell/hie-bios/pull/337)
  * Fixes a bug where hie-bios fails to load cabal cradles with `.ghci` files
* Improve error messages if cabal invocation fails [#338](https://github.com/haskell/hie-bios/pull/338)
* Allow text-2.0 [#335](https://github.com/haskell/hie-bios/pull/335)

## 2022-02-25 - 0.9.0

* Use the proper GHC version given by cabal [#282](https://github.com/haskell/hie-bios/pull/282)
  * In particular, honour the `with-compiler` field in `cabal.project`
* Drop support for GHC 8.4 [#331](https://github.com/haskell/hie-bios/pull/331)

## 2022-01-06 - 0.8.1

* Add support for GHC 9.0.2 [#322](https://github.com/haskell/hie-bios/pull/322)

## 2021-11-29 - 0.8.0

* Support aeson >= 2.0. [#313](https://github.com/haskell/hie-bios/pull/313)
* Remove CradleOpt Type [#293](https://github.com/haskell/hie-bios/pull/293)

## 2021-08-30 - 0.7.6

* Don't look for NIX_GHC_LIBDIR as it is redundant [#294](https://github.com/mpickering/hie-bios/pull/294)
* Add compatibility for GHC 9.0 and 9.2 [#300](https://github.com/mpickering/hie-bios/pull/300)
  * Add CPP statements for IncludeSpecs [#307](https://github.com/mpickering/hie-bios/pull/307)
* Refactor implicit config discovery [#291](https://github.com/mpickering/hie-bios/pull/291)
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
