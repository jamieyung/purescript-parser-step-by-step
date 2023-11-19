module ParserStepByStep.Step4 where

import Prelude

import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Char (fromCharCode)
import Data.Either (Either(..), note)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- Intro -----------------------------------------------------------------------
--------------------------------------------------------------------------------

type ParseError = { idx :: Int, err :: String }

newtype Parser a = Parser (ParserState -> Either ParseError (Tuple a ParserState))

--------------------------------------------------------------------------------
-- Refactoring -----------------------------------------------------------------
--------------------------------------------------------------------------------

chompByte :: Int -> Parser Unit
chompByte expected = Parser \{ idx, arr } -> do
  b <- note { idx, err: "failed to get next byte" } $ A.index arr idx
  when (b /= expected)
    (Left { idx, err: "expected " <> show expected <> " but got " <> show b })
  pure $ Tuple unit { idx: idx + 1, arr }

popByte :: Parser Int
popByte = Parser \{ idx, arr } -> do
  b <- note { idx, err: "failed to get next byte" } $ A.index arr idx
  pure $ Tuple b { idx: idx + 1, arr }

popBytes :: Int -> Parser (Array Int)
popBytes n = Parser \{ idx, arr } -> do
  let bs = A.slice idx (idx + n) arr
  when (A.length bs /= n)
    (Left { idx, err: "Expected " <> show n <> " more bytes in input" })
  pure $ Tuple bs { idx: idx + n, arr }

liftEither :: forall a. Either String a -> Parser a
liftEither e = Parser \st@{ idx } -> do
  x <- lmap (\err -> { idx, err }) e
  pure $ Tuple x st

runParser :: forall a. Parser a -> ParserState -> Either ParseError (Tuple a ParserState)
runParser (Parser p) st = p st

--------------------------------------------------------------------------------
-- Refactoring parseKV and parseFile -------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Analysis --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- The current state of affairs:
-- 1. Explicit array access             Fixed!
-- 2. Excessive memory use              Fixed!
-- 3. Inconvenient parser combination   Fixed!
-- 4. Basic errors                      Fixed!

--------------------------------------------------------------------------------
-- These are the same as in the previous step ----------------------------------
--------------------------------------------------------------------------------

type ParserState = { idx :: Int, arr :: Array Int }

instance Bind Parser where
  bind (Parser p1) f = Parser \st0 -> do
    Tuple result st1 <- p1 st0
    let Parser p2 = f result
    p2 st1

instance Applicative Parser where
  pure a = Parser \st -> Right $ Tuple a st

instance Apply Parser where
  apply (Parser p1) (Parser p2) = Parser \st0 -> do
    Tuple f st1 <- p1 st0
    Tuple x st2 <- p2 st1
    Right $ Tuple (f x) st2

instance Functor Parser where
  map f (Parser p) = Parser \st0 -> do
    Tuple x st1 <- p st0
    Right $ Tuple (f x) st1

type KV = { key :: String, value :: String }

parseKV :: Parser KV
parseKV = do
  chompByte 1
  keyLen <- popByte
  keyBytes <- popBytes keyLen
  key <- liftEither $ stringFromCharArray keyBytes
  valLen <- popByte
  valBytes <- popBytes valLen
  value <- liftEither $ stringFromCharArray valBytes
  pure { key, value }

parseFile :: Parser { version :: Int, kv1 :: KV, kv2 :: KV }
parseFile = do
  version <- popByte
  kv1 <- parseKV
  kv2 <- parseKV
  pure { version, kv1, kv2 }

-- helper function
stringFromCharArray :: Array Int -> Either String String
stringFromCharArray arr = arr
  # traverse (\c -> c # fromCharCode # note ("failed to parse char code: " <> show c))
  <#> fromCharArray
