module Test.ParserStepByStep.Step1 where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import ParserStepByStep.Step1 (parseFile, parseKV)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Step1" do

  describe "parseKV" do

    it "should fail if there is no first byte in the input" do
      parseKV [] `shouldEqual` Left "failed to get next byte"

    it "should fail if the first byte is not 1" do
      parseKV [ 2 ] `shouldEqual` Left "expected 1 but got 2"

    it "should fail if there is no byte for the key length" do
      parseKV [ 1 ] `shouldEqual` Left "failed to get next byte"

    it "should fail if the number of bytes remaining is less than what the key length byte indicates" do
      parseKV [ 1, 10 ] `shouldEqual` Left "Expected 10 more bytes in input"

    it "should fail if there is no byte for the value length" do
      parseKV [ 1, 2, 65, 66 ] `shouldEqual` Left "failed to get next byte"

    it "should fail if the number of bytes remaining is less than what the value length byte indicates" do
      parseKV [ 1, 2, 65, 66, 10 ] `shouldEqual` Left "Expected 10 more bytes in input"

    it "should succeed on valid input" do
      let
        -- the first byte is 1 (the magic byte indicating that what follows is a key-value pair)
        -- the next byte is 2 (indicating that the key is two bytes long)
        -- the next two bytes are the key: [65, 66], or "AB" as UTF8
        -- the next byte is 3 (indicating that the value is three bytes long)
        -- the next two bytes are the value: [100, 101, 102], or "def" as UTF8
        -- the next byte (999) is just something to show that the parser consumes only the key value pair and no more
        input = [ 1, 2, 65, 66, 3, 100, 101, 102, 999 ]
        result = parseKV input
      result `shouldEqual` (Right $ Tuple { key: "AB", value: "def" } [ 999 ])

  describe "parseFile" do

    it "should fail on empty input" do
      parseFile [] `shouldEqual` Left "failed to get next byte"

    it "should succeed on valid input" do
      let
        version = 10
        kv1 = [ 1, 2, 65, 66, 3, 100, 101, 102 ]
        kv2 = [ 1, 3, 65, 66, 67, 2, 103, 103 ]
        input = [ version ] <> kv1 <> kv2 <> [ 999 ]
        result = parseFile input
        expectedResult =
          { version: 10
          , kv1: { key: "AB", value: "def" }
          , kv2: { key: "ABC", value: "gg" }
          }
        expectedRemainingArr = [ 999 ]
      result `shouldEqual` (Right $ Tuple expectedResult expectedRemainingArr)
