{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HIE.Bios.Wrappers (
  cabalWrapper,
  cabalWithReplWrapper,
  cabalWrapperHs,
  cabalWithReplWrapperHs,
) where

import Data.FileEmbed
#if __GLASGOW_HASKELL__ >= 903
  hiding (makeRelativeToProject)
#endif
import Language.Haskell.TH.Syntax

cabalWrapper :: String
cabalWrapper = $(makeRelativeToProject "wrappers/cabal" >>= embedStringFile)

cabalWrapperHs :: String
cabalWrapperHs = $(makeRelativeToProject "wrappers/cabal.hs" >>= embedStringFile)

cabalWithReplWrapper :: String
cabalWithReplWrapper = $(makeRelativeToProject "wrappers/cabal-with-repl" >>= embedStringFile)

cabalWithReplWrapperHs :: String
cabalWithReplWrapperHs = $(makeRelativeToProject "wrappers/cabal-with-repl.hs" >>= embedStringFile)
