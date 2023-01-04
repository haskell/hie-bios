{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module HIE.Bios.Wrappers (cabalWrapper, cabalWrapperHs) where

import Data.FileEmbed hiding (makeRelativeToProject)
import Language.Haskell.TH.Syntax

cabalWrapper :: String
cabalWrapper = $(makeRelativeToProject "wrappers/cabal" >>= embedStringFile)

cabalWrapperHs :: String
cabalWrapperHs = $(makeRelativeToProject "wrappers/cabal.hs" >>= embedStringFile)

