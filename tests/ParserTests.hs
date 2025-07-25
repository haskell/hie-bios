{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import HIE.Bios.Config
#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.Key ( Key )
import qualified Data.Aeson.KeyMap as Map
#else
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
#endif
import Data.Void
import Data.Yaml
import System.FilePath
import Control.Applicative ( (<|>) )
import Control.Exception
import Data.Typeable

configDir :: FilePath
configDir = "tests/configs"

main :: IO ()
main = defaultMain $
  testGroup "Parser Tests"
    [ assertParser "cabal-1.yaml" (noDeps (Cabal $ CabalType (Just "lib:hie-bios") Nothing))
    , assertParser "stack-config.yaml" (noDeps (Stack $ StackType Nothing Nothing))
    --, assertParser "bazel.yaml" (noDeps Bazel)
    , assertParser "bios-1.yaml" (noDeps (Bios (Program "program") Nothing Nothing))
    , assertParser "bios-2.yaml" (noDeps (Bios (Program "program") (Just (Program "dep-program")) Nothing))
    , assertParser "bios-3.yaml" (noDeps (Bios (Command "shellcommand") Nothing Nothing))
    , assertParser "bios-4.yaml" (noDeps (Bios (Command "shellcommand") (Just (Command "dep-shellcommand")) Nothing))
    , assertParser "bios-5.yaml" (noDeps (Bios (Command "shellcommand") (Just (Program "dep-program")) Nothing))
    , assertParser "dependencies.yaml" (Config (CradleConfig ["depFile"] (Cabal $ CabalType (Just "lib:hie-bios") Nothing)))
    , assertParser "direct.yaml" (noDeps (Direct ["list", "of", "arguments"]))
    , assertParser "none.yaml" (noDeps None)
    --, assertParser "obelisk.yaml" (noDeps Obelisk)
    , assertParser "multi.yaml" (noDeps (Multi [("./src", CradleConfig [] (Cabal $ CabalType (Just "lib:hie-bios") Nothing))
                                             ,("./test", CradleConfig [] (Cabal $ CabalType (Just "test") Nothing))]))

    , assertParser "cabal-multi.yaml" (noDeps (CabalMulti (CabalType Nothing Nothing)
                                                        [("./src", CabalType (Just "lib:hie-bios") Nothing)
                                                        ,("./", CabalType (Just "lib:hie-bios") Nothing)]))

    , assertParser "stack-multi.yaml" (noDeps (StackMulti (StackType Nothing Nothing)
                                                        [("./src", StackType (Just "lib:hie-bios") Nothing)
                                                        ,("./", StackType (Just"lib:hie-bios") Nothing)]))

    , assertParser "nested-cabal-multi.yaml" (noDeps (Multi [("./test/testdata", CradleConfig [] None)
                                                          ,("./", CradleConfig [] (
                                                                    CabalMulti (CabalType Nothing Nothing)
                                                                               [("./src", CabalType (Just "lib:hie-bios") Nothing)
                                                                               ,("./tests", CabalType (Just "parser-tests") Nothing)]))]))

    , assertParser "nested-stack-multi.yaml" (noDeps (Multi [("./test/testdata", CradleConfig [] None)
                                                          ,("./", CradleConfig [] (
                                                                    StackMulti (StackType Nothing Nothing)
                                                                               [("./src", StackType (Just "lib:hie-bios") Nothing)
                                                                               ,("./tests", StackType (Just "parser-tests") Nothing)]))]))
    -- Assertions for cabal.project files
    , assertParser "cabal-with-project.yaml"
      (noDeps (Cabal $ CabalType Nothing (Just "cabal.project.extra")))
    , assertParser "cabal-with-both.yaml"
      (noDeps (Cabal $ CabalType (Just "hie-bios:hie") (Just "cabal.project.extra")))
    , assertParser "multi-cabal-with-project.yaml"
      (noDeps (CabalMulti (CabalType Nothing (Just "cabal.project.extra"))
                          [("./src", CabalType (Just "lib:hie-bios") Nothing)
                          ,("./vendor", CabalType (Just "parser-tests") Nothing)]))
    -- Assertions for stack.yaml files
    , assertParser "stack-with-yaml.yaml"
      (noDeps (Stack $ StackType Nothing (Just "stack-8.8.3.yaml")))
    , assertParser "stack-with-both.yaml"
      (noDeps (Stack $ StackType (Just "hie-bios:hie") (Just "stack-8.8.3.yaml")))
    , assertParser "multi-stack-with-yaml.yaml"
      (noDeps (StackMulti (StackType Nothing (Just "stack-8.8.3.yaml"))
                          [("./src", StackType (Just "lib:hie-bios") Nothing)
                          ,("./vendor", StackType (Just "parser-tests") Nothing)]))

    , assertCustomParser "ch-cabal.yaml"
      (noDeps (Other CabalHelperCabal $ simpleCabalHelperYaml "cabal"))
    , assertCustomParser "ch-stack.yaml"
      (noDeps (Other CabalHelperStack $ simpleCabalHelperYaml "stack"))
    , assertCustomParser "multi-ch.yaml"
      (noDeps (Multi
        [ ("./src", CradleConfig [] (Other CabalHelperStack $ simpleCabalHelperYaml "stack"))
        , ("./input", CradleConfig [] (Other CabalHelperCabal $ simpleCabalHelperYaml "cabal"))
        , ("./test", CradleConfig [] (Cabal $ CabalType (Just "test") Nothing))
        , (".", CradleConfig [] None)
        ]))
    , assertParserFails "keys-not-unique-fails.yaml" invalidYamlException
    , assertParser "cabal-empty-config.yaml" (noDeps (Cabal $ CabalType Nothing Nothing))
    ]

assertParser :: FilePath -> Config Void -> TestTree
assertParser fp cc = testCase fp $ do
  conf <- readConfig (configDir </> fp)
  (conf == cc) @? (unlines [("Parser Failed: " ++ fp)
                           , "Expected: " ++ show cc
                           , "Actual: " ++ show conf ])

invalidYamlException :: Selector ParseException
invalidYamlException (InvalidYaml (Just _)) = True
invalidYamlException _ = False

assertParserFails :: Exception e => FilePath -> Selector e -> TestTree
assertParserFails fp es = testCase fp $ (readConfig (configDir </> fp) :: IO (Config Void)) `shouldThrow` es

assertCustomParser :: FilePath -> Config CabalHelper -> TestTree
assertCustomParser fp cc = testCase fp $ do
  conf <- readConfig (configDir </> fp)
  (conf == cc) @? (unlines [("Parser Failed: " ++ fp)
                          , "Expected: " ++ show cc
                          , "Actual: " ++ show conf ])

noDeps :: CradleTree a -> Config a
noDeps c = Config (CradleConfig [] c)

shouldThrow :: (HasCallStack, Exception e) => IO a -> Selector e -> Assertion
shouldThrow act select = do
  r <- try act
  case r of
    Left (exc :: e)
      | select exc -> pure ()
    Left exc -> assertFailure $ "Exception Type is not expected: " ++ exceptionType ++ " (" ++ show exc ++ ")"
    Right _ -> assertFailure $ "did not get expected exception: " ++ exceptionType
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) select
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "ParserTests.shouldThrow: broken Typeable instance"

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

simpleCabalHelperYaml :: Key -> Value
simpleCabalHelperYaml tool =
  object
    [ ( "cabal-helper", object
        [ (tool, Null)
        ]
      )
    ]

type Selector a = a -> Bool

-- ------------------------------------------------------------------
-- Helper functions to support aeson < 2
-- ------------------------------------------------------------------

#if !MIN_VERSION_aeson(2,0,0)
type Key = T.Text
#endif
