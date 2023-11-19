module ParserStepByStep.Step1 where

import Prelude

import Data.Array as A
import Data.Char (fromCharCode)
import Data.Either (Either(..), note)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- Intro -----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- This step involves a refactoring of `parseKV` to eliminate the need for index
-- arithmetic. This requires a small shift in approach: each access of the array
-- will now consume the bytes and return the remainder before the next step
-- (effectively we're just chopping the `parseKV` function into many smaller
-- parsers). This means that each access can just start reading at the start of
-- the array, thereby eliminating the arithmetic.

--------------------------------------------------------------------------------
-- First, some util functions --------------------------------------------------
--------------------------------------------------------------------------------

-- It is convenient to write some utility functions to assist in this process.
-- The actual direct array access and manipulation will be contained to these
-- helper functions, making the larger parsers more readable and maintainable.

-- chompByte is a function that is useful for when we want to assert that the
-- next byte is a certain value, and then we want to skip that byte. It simply
-- asserts that the head of the array is the expected byte, and returns `unit`
-- and the tail of the array. In the refactored version of `parseKV` below, it
-- is used to check that the next byte is 0x1.
chompByte :: Int -> Parser Unit
chompByte expected = \arr -> do
  { head, tail } <- note "failed to get next byte" $ A.uncons arr
  when (head /= expected) (Left $ "expected " <> show expected <> " but got " <> show head)
  pure $ Tuple unit tail

-- Note that chompByte could be made more general:
-- ```purescript
-- satisfy :: (Int -> Boolean) -> Parser Unit
-- satisfy p = \arr -> do
--   { head, tail } <- note "failed to get next byte" $ A.uncons arr
--   when (not $ p head) (Left "next byte failed the satisfy condition")
--   pure $ Tuple unit tail
--
-- chompByte :: Int -> Parser Unit
-- chompByte b = satisfy (_ == b)
-- ```
-- For simplicity, this has been skipped here (It's a good idea for a proper
-- parser though).

-- popByte is a function that is useful for when we just want to grab the next
-- byte in the array and then move on. In the refactored version of `parseKV`
-- below, it is used to get the length byte for the key and value.
popByte :: Parser Int
popByte = \arr -> do
  { head, tail } <- note "failed to get next byte" $ A.uncons arr
  pure $ Tuple head tail

-- popBytes is a function that is useful for when we want to grab the next n
-- bytes in the array and move on. In the refactored version of `parseKV` below,
-- it is used to get the bytes for the key and value.
popBytes :: Int -> Parser (Array Int)
popBytes n = \arr -> do
  let bs = A.take n arr
  when (A.length bs /= n) (Left $ "Expected " <> show n <> " more bytes in input")
  pure $ Tuple bs (A.drop n arr)

--------------------------------------------------------------------------------
-- The refactored parseKV function ---------------------------------------------
--------------------------------------------------------------------------------

-- Using these util functions, we can re-write parseKV to be much more concise:

parseKV :: Parser KV
parseKV = \arr0 -> do
  -- 1. check the first byte is 1
  Tuple _ arr1 <- chompByte 1 arr0

  -- 2. get the length of the key
  Tuple keyLen arr2 <- popByte arr1

  -- 3. get the key
  Tuple keyBytes arr3 <- popBytes keyLen arr2
  key <- stringFromCharArray keyBytes

  -- 4. get the length of the value
  Tuple valLen arr4 <- popByte arr3

  -- 5. get the value
  Tuple valBytes arr5 <- popBytes valLen arr4
  value <- stringFromCharArray valBytes

  pure $ Tuple { key, value } arr5

parseFile :: Parser { version :: Int, kv1 :: KV, kv2 :: KV }
parseFile = \arr0 -> do
  Tuple version arr1 <- popByte arr0 -- using popByte here instead of A.uncons
  Tuple kv1 arr2 <- parseKV arr1
  Tuple kv2 arr3 <- parseKV arr2
  pure $ Tuple { version, kv1, kv2 } arr3

--------------------------------------------------------------------------------
-- Analysis --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Notice that all the `A.slice` and `A.index` calls are gone, as well as the
-- need to calculate offsets and do index arithmetic. Much nicer! It's therefore
-- also slightly easier to refactor; inserting a new parsing step in the middle
-- would now no longer require updating a bunch of arithmetic.

-- However, it would require updating a bunch of variable names now that we've
-- got these `arr1` `arr2` etc variables everywhere. Also, this approach seems
-- less efficient than the previous one because there are now more places where
-- we're constructing new arrays (and therefore doing more memory copying).

-- The current state of affairs:
-- 1. Explicit array access             Fixed!
-- 2. Excessive memory use              We've made it worse (temporarily)!
-- 3. Inconvenient parser combination   Haven't addressed this yet
-- 4. Basic errors                      Haven't addressed this yet

-- Next up let's fix issue 2 (excessive memory use). On to Step2!

--------------------------------------------------------------------------------
-- These are the same as in the previous step ----------------------------------
--------------------------------------------------------------------------------

type Parser a = Array Int -> Either String (Tuple a (Array Int))

type KV = { key :: String, value :: String }

-- helper function
stringFromCharArray :: Array Int -> Either String String
stringFromCharArray arr = arr
  # traverse (\c -> c # fromCharCode # note ("failed to parse char code: " <> show c))
  <#> fromCharArray
