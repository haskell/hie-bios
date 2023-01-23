{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module HIE.Bios.Wrappers (cabalWrapper, cabalWrapperHs) where

import Data.FileEmbed
#if __GLASGOW_HASKELL__ >= 903
  hiding (makeRelativeToProject)
#endif
import Language.Haskell.TH.Syntax

cabalWrapper :: String
cabalWrapper = $(makeRelativeToProject "wrappers/cabal" >>= embedStringFile)

cabalWrapperHs :: String
cabalWrapperHs = $(makeRelativeToProject "wrappers/cabal.hs" >>= embedStringFile)

