import           Test.Hspec
import           Packages                       ( processPackageList )
import           Registry                       ( Source(..)
                                                , Package(..)
                                                , createRegistry
                                                )


main :: IO ()
main = hspec testSuite


getNames = fmap name

noop = Custom $ return ()

b = Package
    { name         = "b"
    , source       = noop
    , dependencies = [ Package
                         { name         = "b1"
                         , source       = noop
                         , dependencies = []
                         , config       = Nothing
                         , snippets     = []
                         }
                     , Package
                         { name         = "b2"
                         , source       = noop
                         , dependencies = []
                         , config       = Nothing
                         , snippets     = []
                         }
                     ]
    , config       = Nothing
    , snippets     = []
    }

c = Package
    { name         = "c"
    , source       = noop
    , dependencies = [ Package
                         { name         = "c1"
                         , source       = noop
                         , dependencies = []
                         , config       = Nothing
                         , snippets     = []
                         }
                     , Package
                         { name         = "c2"
                         , source       = noop
                         , dependencies = []
                         , config       = Nothing
                         , snippets     = []
                         }
                     , b
                     ]
    , config       = Nothing
    , snippets     = []
    }

testRegistry = createRegistry
    [ Package
        { name         = "no-dependencies"
        , source       = noop
        , dependencies = []
        , config       = Nothing
        , snippets     = []
        }
    , Package
        { name         = "a"
        , source       = noop
        , dependencies = [b, c]
        , config       = Nothing
        , snippets     = []
        }
    , b
    , c
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
