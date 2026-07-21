module Test.TomlSpec (spec) where

import qualified Data.Map as Map
import Data.String (fromString)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Toml

spec :: Spec
spec = do
  describe "tables" $ do
    it "success" $ do
      let
        input =
          unlines
            [ "[header]"
            , "key_1 = \"value_1\""
            , "key_2 = \"value_2\""
            , "key_3 = \"value_3\""
            ]

        decoder =
          Toml.table (fromString "header") $
            (,,)
              <$> Toml.key (fromString "key_1") Toml.string
              <*> Toml.key (fromString "key_2") Toml.string
              <*> Toml.key (fromString "key_3") Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Right ("value_1", "value_2", "value_3")

    it "duplicate" $ do
      let
        input1 =
          concatMap
            (++ "\n")
            [ "[header]"
            , "key_1 = \"value_1\""
            , "key_2 = \"value_2\""
            , "key_3 = \"value_3\""
            , ""
            ]

        input2 =
          concatMap
            (++ "\n")
            [ "[header]"
            , "key_1 = \"value_1\""
            , ""
            ]

        input3 =
          concatMap
            (++ "\n")
            [ "[other-header]"
            , "key_1 = \"value_1\""
            , ""
            ]

        input4 =
          unlines
            [ "[header]"
            , "key_2 = \"value_2\""
            ]

        input = concat [input1, input2, input3, input4]

        decoder =
          Toml.table (fromString "header") $
            (,,)
              <$> Toml.key (fromString "key_1") Toml.string
              <*> Toml.key (fromString "key_2") Toml.string
              <*> Toml.key (fromString "key_3") Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Left
          ( Toml.DuplicateTables
              [length input1, length input1 + length input2 + length input3]
              (fromString "header")
          )

    it "missing" $ do
      let
        input =
          unlines
            [ "[other-header]"
            , "key_1 = \"value_1\""
            , "key_2 = \"value_2\""
            , "key_3 = \"value_3\""
            ]

        decoder =
          Toml.table (fromString "header") $
            (,,)
              <$> Toml.key (fromString "key_1") Toml.string
              <*> Toml.key (fromString "key_2") Toml.string
              <*> Toml.key (fromString "key_3") Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Left (Toml.MissingTable 0 $ fromString "header")

    it "unexpected" $ do
      let
        input1 =
          concatMap
            (++ "\n")
            [ "[header]"
            , "key_1 = \"value_1\""
            , "key_2 = \"value_2\""
            , "key_3 = \"value_3\""
            , ""
            ]

        input2 =
          unlines
            [ "[other-header]"
            , "key_1 = \"value_1\""
            ]

        input = input1 ++ input2

        decoder =
          Toml.table (fromString "header") $
            (,,)
              <$> Toml.key (fromString "key_1") Toml.string
              <*> Toml.key (fromString "key_2") Toml.string
              <*> Toml.key (fromString "key_3") Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Left (Toml.UnexpectedEntries mempty [length input1])

  describe "keys" $ do
    it "success" $ do
      let
        input =
          unlines
            [ "[header]"
            , "key_1 = \"value_1\""
            , "key_2 = \"value_2\""
            , "key_3 = \"value_3\""
            ]

        decoder =
          Toml.table (fromString "header") $
            Toml.keys Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Right
          ( Map.fromList
              [ (fromString "key_1", fromString "value_1")
              , (fromString "key_2", fromString "value_2")
              , (fromString "key_3", fromString "value_3")
              ]
          )

    it "duplicate" $ do
      let
        input1 =
          concatMap
            (++ "\n")
            [ "[header]"
            , "key_1 = \"value_1\""
            ]

        input2 =
          unlines
            [ "key_1 = \"value_2\""
            , "key_3 = \"value_3\""
            ]

        input = input1 ++ input2

        decoder =
          Toml.table (fromString "header") $
            Toml.keys Toml.string

      ( do
          toml <- Toml.parse $ fromString input
          Toml.decode toml decoder
        )
        `shouldBe` Left
          (Toml.DuplicateKey (length input1) (fromString "key_1"))
