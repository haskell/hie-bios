{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HIE.Bios.Ghc.Types where


import qualified Exception as GE
import GHC (Ghc)

import Control.Exception (IOException)
import Control.Applicative (Alternative(..))


instance Alternative Ghc where
    x <|> y = x `GE.gcatch` (\(_ :: IOException) -> y)
    empty = undefined
