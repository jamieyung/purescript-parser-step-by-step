module ParserStepByStep.Step0 where

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

-- The general shape of a parser is it takes the remaining input (the array) and
-- returns either an error or the parse result `a` plus the new remaining input,
-- with the parsed bytes chopped off:

type Parser a = Array Int -> Either String (Tuple a (Array Int))

--------------------------------------------------------------------------------
-- An example ------------------------------------------------------------------
--------------------------------------------------------------------------------

type KV = { key :: String, value :: String }

-- `parseKV` expects key-value pairs to be in this format:
-- 1. The first byte is 0x1, indicating a key value pair
-- 2. The second byte indicates the length of the key (max of 255 bytes)
-- 3. The next n bytes are the UTF8 characters of the key
-- 4. The next byte indicates the length of the value (max of 255 bytes)
-- 5. The next n bytes are the UTF8 characters of the value
parseKV :: Parser KV
parseKV = \arr -> do -- this operates in the `Either String` monad
  -- 1. check the first byte is 1
  b0 <- note "failed to get first byte" $ A.index arr 0
  when (b0 /= 1) (Left "not a key value pair")

  -- 2. get the length of the key
  keyLen <- note "failed to get key length byte" $ A.index arr 1

  -- 3. get the key
  let keyBytes = A.slice 2 (2 + keyLen) arr
  when (A.length keyBytes /= keyLen) (Left "not enough bytes in key")
  key <- stringFromCharArray keyBytes

  -- 4. get the length of the value
  let valLenIdx = 2 + keyLen
  valLen <- note "failed to get value length byte" $ A.index arr valLenIdx

  -- 5. get the value
  let
    valBytesIdx = valLenIdx + 1
    valBytes = A.slice valBytesIdx (valBytesIdx + valLen) arr
  when (A.length valBytes /= valLen) (Left "not enough bytes in value")
  value <- stringFromCharArray valBytes

  pure $ Tuple { key, value } (A.drop (2 + keyLen + 1 + valLen) arr)

-- helper function
stringFromCharArray :: Array Int -> Either String String
stringFromCharArray arr = arr
  # traverse (\c -> c # fromCharCode # note ("failed to parse char code: " <> show c))
  <#> fromCharArray

parseFile :: Parser { version :: Int, kv1 :: KV, kv2 :: KV }
parseFile = \arr0 -> do
  { head: version, tail: arr1 } <- note "oh no" (A.uncons arr0)
  Tuple kv1 arr2 <- parseKV arr1
  Tuple kv2 arr3 <- parseKV arr2
  pure $ Tuple { version, kv1, kv2 } arr3

--------------------------------------------------------------------------------
-- Analysis --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Writing parsers this way is fine, but there are some pain points:

-- 1. Explicit array access
-- The `parseKV` function pulls elements and slices out of the array in several
-- spots using `A.index` and `A.slice`. This is error-prone; it's easy to get
-- the index arithmetic wrong! It's also difficult to modify, eg. adding an
-- extra parse step in the middle would mean all the indices afterwards would
-- need to be updated. Maintenance hell!

-- 2. Excessive memory use
-- This approach involves passing the remainder of the array out at the end of
-- each parser function, usually constructed with `A.drop`. This creates a new
-- array in memory each time. With a large input array, this would be a lot of
-- memory allocation/copying and could noticeably slow down the program.

-- 3. Inconvenient parser combination
-- Combining parsers is not particularly ergonomic (see `parseFile`). Because
-- each call to a parser returns the result and the new remainder array, this
-- approach results in lots of variables named `arr1`, arr2`, etc. It's very
-- easy to make a mistake and accidentally pass the wrong variable somewhere!

-- 4. Basic errors (minor issue)
-- If an error is returned, there's no way to tell where in the input the error
-- occurred.

-- Over the following steps, we will gradually address and fix these pain points.

-- On to Step1!
