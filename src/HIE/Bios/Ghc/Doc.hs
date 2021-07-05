{-# LANGUAGE CPP #-}
-- | Pretty printer utilities
module HIE.Bios.Ghc.Doc where


import GHC (DynFlags, getPrintUnqual, pprCols, GhcMonad)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.Session (initSDocContext)
import GHC.Utils.Outputable (PprStyle, SDoc, runSDoc, neverQualify, )
import GHC.Utils.Ppr  (Mode(..), Doc, Style(..), renderStyle, style)
#else
import Outputable (PprStyle, SDoc, runSDoc, neverQualify, initSDocContext)
import Pretty (Mode(..), Doc, Style(..), renderStyle, style)
#endif

import HIE.Bios.Ghc.Gap (makeUserStyle, pageMode, oneLineMode)

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag stl sdoc = showDocWith dflag pageMode $ runSDoc sdoc scontext
  where
    scontext = initSDocContext dflag stl

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag stl sdoc = showDocWith dflag oneLineMode $ runSDoc sdoc scontext
  where
    scontext = initSDocContext dflag stl

getStyle :: (GhcMonad m) => DynFlags -> m PprStyle
getStyle dflags = makeUserStyle dflags <$> getPrintUnqual

styleUnqualified :: DynFlags -> PprStyle
styleUnqualified dflags = makeUserStyle dflags neverQualify

showDocWith :: DynFlags -> Mode -> Doc -> String
showDocWith dflags md = renderStyle mstyle
  where
    mstyle = style { mode = md, lineLength = pprCols dflags }
