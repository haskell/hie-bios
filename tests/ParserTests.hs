module Main where

import Test.Tasty
import Test.Tasty.HUnit
import HIE.Bios.Config
import System.FilePath

configDir :: FilePath
configDir = "tests/configs"

main :: IO ()
main = defaultMain $
  testCase "Parser Tests" $ do
    assertParser "cabal-1.yaml" (noDeps (Cabal (Just "lib:hie-bios")))
    assertParser "stack-config.yaml" (noDeps Stack)
    --assertParser "bazel.yaml" (noDeps Bazel)
    assertParser "bios-1.yaml" (noDeps (Bios "program" Nothing))
    assertParser "bios-2.yaml" (noDeps (Bios "program" (Just "dep-program")))
    assertParser "dependencies.yaml" (Config (CradleConfig ["depFile"] (Cabal (Just "lib:hie-bios"))))
    assertParser "direct.yaml" (noDeps (Direct ["list", "of", "arguments"]))
    assertParser "none.yaml" (noDeps None)
    --assertParser "obelisk.yaml" (noDeps Obelisk)
    assertParser "multi.yaml" (noDeps (Multi [("./src", CradleConfig [] (Cabal (Just "lib:hie-bios")))
                                             , ("./test", CradleConfig [] (Cabal (Just "test")) ) ]))

    assertParser "cabal-multi.yaml" (noDeps (CabalMulti [("./src", "lib:hie-bios")
                                                    ,("./", "lib:hie-bios")]))

    assertParser "nested-cabal-multi.yaml" (noDeps (Multi [("./test/testdata", CradleConfig [] None)
                                                          ,("./", CradleConfig [] (
                                                                    CabalMulti [("./src", "lib:hie-bios")
                                                                               ,("./tests", "parser-tests")]))]))

assertParser :: FilePath -> Config -> Assertion
assertParser fp cc = do
  conf <- readConfig (configDir </> fp)
  (conf == cc) @? (unlines [("Parser Failed: " ++ fp)
                           , "Expected: " ++ show cc
                           , "Actual: " ++ show conf ])

noDeps :: CradleType -> Config
noDeps c = Config (CradleConfig [] c)
