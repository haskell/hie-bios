{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import HIE.Bios.Config
import qualified Data.HashMap.Strict as Map
import Data.Void
import Data.Yaml
import qualified Data.Text as T
import System.FilePath
import Control.Applicative ( (<|>) )

configDir :: FilePath
configDir = "tests/configs"

main :: IO ()
main = defaultMain $
  testCase "Parser Tests" $ do
    assertParser "cabal-1.yaml" (noDeps (Cabal (Just "lib:hie-bios")))
    assertParser "stack-config.yaml" (noDeps (Stack Nothing Nothing))
    --assertParser "bazel.yaml" (noDeps Bazel)
    assertParser "bios-1.yaml" (noDeps (Bios (Program "program") Nothing))
    assertParser "bios-2.yaml" (noDeps (Bios (Program "program") (Just (Program "dep-program"))))
    assertParser "bios-3.yaml" (noDeps (Bios (Command "shellcommand") Nothing))
    assertParser "bios-4.yaml" (noDeps (Bios (Command "shellcommand") (Just (Command "dep-shellcommand"))))
    assertParser "bios-5.yaml" (noDeps (Bios (Command "shellcommand") (Just (Program "dep-program"))))
    assertParser "dependencies.yaml" (Config (CradleConfig ["depFile"] (Cabal (Just "lib:hie-bios"))))
    assertParser "direct.yaml" (noDeps (Direct ["list", "of", "arguments"]))
    assertParser "none.yaml" (noDeps None)
    --assertParser "obelisk.yaml" (noDeps Obelisk)
    assertParser "multi.yaml" (noDeps (Multi [("./src", CradleConfig [] (Cabal (Just "lib:hie-bios")))
                                             , ("./test", CradleConfig [] (Cabal (Just "test")) ) ]))

    assertParser "cabal-multi.yaml" (noDeps (CabalMulti [("./src", CabalType $ Just "lib:hie-bios")
                                                        ,("./", CabalType $ Just "lib:hie-bios")]))

    assertParser "stack-multi.yaml" (noDeps (StackMulti [("./src", StackType (Just "lib:hie-bios") Nothing)
                                                        ,("./", StackType (Just"lib:hie-bios") Nothing)]))

    assertParser "nested-cabal-multi.yaml" (noDeps (Multi [("./test/testdata", CradleConfig [] None)
                                                          ,("./", CradleConfig [] (
                                                                    CabalMulti [("./src", CabalType $ Just "lib:hie-bios")
                                                                               ,("./tests", CabalType $ Just "parser-tests")]))]))

    assertParser "nested-stack-multi.yaml" (noDeps (Multi [("./test/testdata", CradleConfig [] None)
                                                          ,("./", CradleConfig [] (
                                                                    StackMulti [("./src", StackType (Just "lib:hie-bios") Nothing)
                                                                              ,("./tests", StackType (Just "parser-tests") Nothing)]))]))

    assertCustomParser "ch-cabal.yaml"
      (noDeps (Other CabalHelperCabal $ simpleCabalHelperYaml "cabal"))
    assertCustomParser "ch-stack.yaml"
      (noDeps (Other CabalHelperStack $ simpleCabalHelperYaml "stack"))
    assertCustomParser "multi-ch.yaml"
      (noDeps (Multi
        [ ("./src", CradleConfig [] (Other CabalHelperStack $ simpleCabalHelperYaml "stack"))
        , ("./input", CradleConfig [] (Other CabalHelperCabal $ simpleCabalHelperYaml "cabal"))
        , ("./test", CradleConfig [] (Cabal (Just "test")))
        , (".", CradleConfig [] None)
        ]))

assertParser :: FilePath -> Config Void -> Assertion
assertParser fp cc = do
  conf <- readConfig (configDir </> fp)
  (conf == cc) @? (unlines [("Parser Failed: " ++ fp)
                           , "Expected: " ++ show cc
                           , "Actual: " ++ show conf ])

assertCustomParser :: FilePath -> Config CabalHelper -> Assertion
assertCustomParser fp cc = do
  conf <- readConfig (configDir </> fp)
  (conf == cc) @? (unlines [("Parser Failed: " ++ fp)
                          , "Expected: " ++ show cc
                          , "Actual: " ++ show conf ])

noDeps :: CradleType a -> Config a
noDeps c = Config (CradleConfig [] c)

-- ------------------------------------------------------------------

data CabalHelper
  = CabalHelperCabal
  | CabalHelperStack
  deriving (Show, Eq)

instance FromJSON CabalHelper where
  parseJSON (Object o)
    | Just obj <- Map.lookup "cabal-helper" o = chCabal obj <|> chStack obj
    where
      chCabal (Object val)
        | Just _val <- Map.lookup "cabal" val = return CabalHelperCabal
      chCabal _ = fail "CH: not a cabal cradle."

      chStack (Object val)
        | Just _val <- Map.lookup "stack" val = return CabalHelperStack
      chStack _ = fail "CH: not a stack cradle."

  parseJSON _ = fail "Not a valid cabal-helper specification"

simpleCabalHelperYaml :: T.Text -> Value
simpleCabalHelperYaml tool =
  object
    [ ( "cabal-helper", object
        [ (tool, Null)
        ]
      )
    ]
