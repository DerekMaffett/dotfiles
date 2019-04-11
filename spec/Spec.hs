import           Test.Hspec
import           Packages                       ( processPackageList
                                                , getConfigsAndSnippets
                                                , stringSources
                                                )
import           Registry                       ( Source(..)
                                                , Package(..)
                                                , Snippet(..)
                                                , SymlinkTarget(..)
                                                , PackageConfig(..)
                                                , createRegistry
                                                , centralRegistry
                                                )


main :: IO ()
main = hspec testSuite


getNames = fmap name

noop = Custom $ return ()

basePackage = Package
    { name         = ""
    , source       = noop
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }

b = basePackage
    { name         = "b"
    , dependencies = [basePackage { name = "b1" }, basePackage { name = "b2" }]
    }

c = basePackage
    { name         = "c"
    , dependencies = [ basePackage { name = "c1" }
                     , basePackage { name = "c2" }
                     , b
                     ]
    }

config1 = PackageConfig "config1" Home
config2 = PackageConfig "config2" Home

testRegistry = createRegistry
    [ basePackage { name = "no-dependencies" }
    , basePackage { name = "a", dependencies = [b, c] }
    , b
    , c
    , basePackage { name = "has-config-1", config = Just config1 }
    , basePackage { name     = "has-snippet-1a"
                  , snippets = [Snippet config1 "well"]
                  }
    , basePackage { name     = "has-snippet-1b"
                  , snippets = [Snippet config1 "hello"]
                  }
    , basePackage { name = "has-config-2", config = Just config2 }
    , basePackage
        { name     = "has-snippet-2a-1c"
        , snippets = [Snippet config1 "there", Snippet config2 "General"]
        }
    , basePackage { name     = "has-snippet-2b"
                  , snippets = [Snippet config2 "Kenobi"]
                  }
    ]

testSuite = do
    describe "processPackageList" $ do
        it "locates packages in the repository"
            $ getNames (processPackageList testRegistry ["no-dependencies"])
            `shouldBe` ["no-dependencies"]
        it "removes packages not in repository"
            $          getNames (processPackageList testRegistry ["missing"])
            `shouldBe` []
        it "removes duplicate packages in initial list"
            $          getNames
                           (processPackageList testRegistry
                                               ["no-dependencies", "no-dependencies"]
                           )
            `shouldBe` ["no-dependencies"]
        it "removes duplicate packages in initial list"
            $          getNames
                           (processPackageList testRegistry
                                               ["no-dependencies", "no-dependencies"]
                           )
            `shouldBe` ["no-dependencies"]
        it "unpacks dependencies in correct order without duplicates"
            $          getNames (processPackageList testRegistry ["a", "b"])
            `shouldBe` ["b1", "b2", "b", "c1", "c2", "c", "a"]

    describe "getConfigsAndSnippets" $ do
        let list =
                [ "has-config-1"
                , "has-snippet-1a"
                , "has-snippet-1b"
                , "has-config-2"
                , "has-snippet-2a-1c"
                , "has-snippet-2b"
                ]
        it "assertion on package list"
            $          getNames (processPackageList testRegistry list)
            `shouldBe` list
        it "checking" $ getConfigsAndSnippets (processPackageList centralRegistry stringSources) `shouldBe` []
        it "should collect configs and snippets together"
            $ getConfigsAndSnippets (processPackageList testRegistry list)
            `shouldBe` [ ( config1
                         , [ Snippet config1 "well"
                           , Snippet config1 "hello"
                           , Snippet config1 "there"
                           ]
                         )
                       , ( config2
                         , [Snippet config2 "General", Snippet config2 "Kenobi"]
                         )
                       ]
