{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HIE.Bios.Wrappers (
  cabalWrapper,
  cabalWithReplWrapper,
  cabalWrapperHs,
  cabalWithReplWrapperHs,
) where

import Data.FileEmbed ( embedStringFile )
import Language.Haskell.TH.Syntax ( makeRelativeToProject )

cabalWrapper :: String
cabalWrapper = $(makeRelativeToProject "wrappers/cabal" >>= embedStringFile)

cabalWrapperHs :: String
cabalWrapperHs = $(makeRelativeToProject "wrappers/cabal.hs" >>= embedStringFile)

cabalWithReplWrapper :: String
cabalWithReplWrapper = $(makeRelativeToProject "wrappers/cabal-with-repl" >>= embedStringFile)

cabalWithReplWrapperHs :: String
cabalWithReplWrapperHs = $(makeRelativeToProject "wrappers/cabal-with-repl.hs" >>= embedStringFile)
