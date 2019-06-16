@ECHO OFF
IF "%i" == "--interactive" (
  ECHO %CD%
  ECHO %*
) ELSE (
  ghc %*
)
