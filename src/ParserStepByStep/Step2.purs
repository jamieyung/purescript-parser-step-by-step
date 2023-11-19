module ParserStepByStep.Step2 where

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

-- This step completely removes the repeated copying of the input array with a
-- simple refactoring; instead of passing the remaining bytes around, we instead
-- pass around the whole array (which is just a reference to the same section of
-- memory) along with an index into the array, and instead of dropping consumed
-- bytes from the array itself (which involves copying the remainder to a new
-- section of memory), we simply advance the index.

--------------------------------------------------------------------------------
-- Updating the Parser type ----------------------------------------------------
--------------------------------------------------------------------------------

-- The `ParserState` type represents the state:
type ParserState = { idx :: Int, arr :: Array Int }

-- The `Parser` type is slightly updated (the only change is to replace
-- `(Array a)` with `ParserState`:
type Parser a = ParserState -> Either String (Tuple a ParserState)

--------------------------------------------------------------------------------
-- Refactoring -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- Only the functions that previously touched the array directly need updating:

chompByte :: Int -> Parser Unit
chompByte expected = \{ idx, arr } -> do
  b <- note "failed to get next byte" $ A.index arr idx
  when (b /= expected) (Left $ "expected " <> show expected <> " but got " <> show b)
  pure $ Tuple unit { idx: idx + 1, arr }

popByte :: Parser Int
popByte = \{ idx, arr } -> do
  b <- note "failed to get next byte" $ A.index arr idx
  pure $ Tuple b { idx: idx + 1, arr }

popBytes :: Int -> Parser (Array Int)
popBytes n = \{ idx, arr } -> do
  let bs = A.slice idx (idx + n) arr
  when (A.length bs /= n) (Left $ "Expected " <> show n <> " more bytes in input")
  pure $ Tuple bs { idx: idx + n, arr }

-- Note that the `parseKV` and `parseFile` functions didn't need to be changed
-- at all, because they're defined in terms of these helper functions!
-- (well technically I renamed the `arr` variables to `st` variables, but that's
-- just a rename).

--------------------------------------------------------------------------------
-- Analysis --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Not much to say; a fairly straightforward refactor and the memory copying is
-- gone!

-- However, we still have these pesky `st` variables. They are quite tedious and
-- error-prone; as previously mentioned, if a new step is inserted before a long
-- list of calls, each of those `st` variables will have to have their number
-- incremented. It's very easy to make a mistake, and really all we're doing is
-- passing the state onto the next call. It should be possible to make the
-- computer do it! In fact, that's what we'll do next :)

-- The current state of affairs:
-- 1. Explicit array access             Fixed!
-- 2. Excessive memory use              Fixed!
-- 3. Inconvenient parser combination   We'll address this next!
-- 4. Basic errors                      Haven't addressed this yet

-- On to Step3!

--------------------------------------------------------------------------------
-- These are the same as in the previous step ----------------------------------
--------------------------------------------------------------------------------

type KV = { key :: String, value :: String }

-- just renamed the `arr` variables to `st`
parseKV :: Parser KV
parseKV = \st0 -> do
  -- 1. check the first byte is 1
  Tuple _ st1 <- chompByte 1 st0

  -- 2. get the length of the key
  Tuple keyLen st2 <- popByte st1

  -- 3. get the key
  Tuple keyBytes st3 <- popBytes keyLen st2
  key <- stringFromCharArray keyBytes

  -- 4. get the length of the value
  Tuple valLen st4 <- popByte st3

  -- 5. get the value
  Tuple valBytes st5 <- popBytes valLen st4
  value <- stringFromCharArray valBytes

  pure $ Tuple { key, value } st5

-- just renamed the `arr` variables to `st`
parseFile :: Parser { version :: Int, kv1 :: KV, kv2 :: KV }
parseFile = \st -> do
  Tuple version st1 <- popByte st
  Tuple kv1 st2 <- parseKV st1
  Tuple kv2 st3 <- parseKV st2
  pure $ Tuple { version, kv1, kv2 } st3

-- helper function
stringFromCharArray :: Array Int -> Either String String
stringFromCharArray arr = arr
  # traverse (\c -> c # fromCharCode # note ("failed to parse char code: " <> show c))
  <#> fromCharArray
