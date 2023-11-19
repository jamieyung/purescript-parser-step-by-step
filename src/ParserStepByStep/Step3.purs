module ParserStepByStep.Step3 where

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

-- This step is likely the one that will feel like the largest jump, and will
-- require the most work to understand.

-- The key to hiding away the explicit state plumbing is to shift it into a
-- `Bind` typeclass instance! That will allow us to use do-notation without
-- explicitly mentioning the state, and the compiler will handle plumbing it
-- through for us.

--------------------------------------------------------------------------------
-- Making Parser a newtype and defining the required instances -----------------
--------------------------------------------------------------------------------

-- Purescript only allows typeclass instances to be defined on newtypes or data
-- types. This means we need to wrap our Parser in a newtype:
newtype Parser a = Parser (ParserState -> Either String (Tuple a ParserState))

-- Now we can define our instances! The `Bind` instance and the `Applicative`
-- instance are the most important ones; the `Bind` instance allows us to use
-- do-notation, and the `Applicative` instance allows us to use `pure`.

-- The `Bind` instance does what we were previously doing manually: it takes the
-- result of the first parser, and passes it through to the rest of the do-block
-- along with the the remaining input (s1).
instance Bind Parser where
  bind (Parser p1) f = Parser \st0 -> do
    Tuple result st1 <- p1 st0
    let Parser p2 = f result
    p2 st1

-- The `Applicative` instance returns the value without consuming any input.
instance Applicative Parser where
  pure a = Parser \st -> Right $ Tuple a st

-- The `Apply` and `Functor` instances are less crucial, but they must be
-- defined because the `Bind` instance will only compile if there's also an
-- `Apply` instance, and the `Apply` instance requires a `Functor` instance.

instance Apply Parser where
  apply (Parser p1) (Parser p2) = Parser \st0 -> do
    Tuple f st1 <- p1 st0
    Tuple x st2 <- p2 st1
    Right $ Tuple (f x) st2

instance Functor Parser where
  map f (Parser p) = Parser \st0 -> do
    Tuple x st1 <- p st0
    Right $ Tuple (f x) st1

--------------------------------------------------------------------------------
-- Refactoring -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- The utility functions just need to be wrapped in the `Parser` constructor
-- (they're otherwise unchanged):

chompByte :: Int -> Parser Unit
chompByte expected = Parser \{ idx, arr } -> do
  b <- note "failed to get next byte" $ A.index arr idx
  when (b /= expected) (Left $ "expected " <> show expected <> " but got " <> show b)
  pure $ Tuple unit { idx: idx + 1, arr }

popByte :: Parser Int
popByte = Parser \{ idx, arr } -> do
  b <- note "failed to get next byte" $ A.index arr idx
  pure $ Tuple b { idx: idx + 1, arr }

popBytes :: Int -> Parser (Array Int)
popBytes n = Parser \{ idx, arr } -> do
  let bs = A.slice idx (idx + n) arr
  when (A.length bs /= n) (Left $ "Expected " <> show n <> " more bytes in input")
  pure $ Tuple bs { idx: idx + n, arr }

-- We introduce a couple new utility functions:

-- | Lifts an Either String into the Parser monad.
liftEither :: forall a. Either String a -> Parser a
liftEither e = Parser \st -> do
  x <- e
  pure $ Tuple x st

-- | Runs the parser on the provided state.
runParser :: forall a. Parser a -> ParserState -> Either String (Tuple a ParserState)
runParser (Parser p) st = p st

--------------------------------------------------------------------------------
-- Refactoring parseKV and parseFile -------------------------------------------
--------------------------------------------------------------------------------

parseKV :: Parser KV
parseKV = do
  -- 1. check the first byte is 1
  chompByte 1

  -- 2. get the length of the key
  keyLen <- popByte

  -- 3. get the key
  keyBytes <- popBytes keyLen
  key <- liftEither $ stringFromCharArray keyBytes

  -- 4. get the length of the value
  valLen <- popByte

  -- 5. get the value
  valBytes <- popBytes valLen
  value <- liftEither $ stringFromCharArray valBytes

  pure { key, value }

parseFile :: Parser { version :: Int, kv1 :: KV, kv2 :: KV }
parseFile = do
  version <- popByte
  kv1 <- parseKV
  kv2 <- parseKV
  pure { version, kv1, kv2 }

--------------------------------------------------------------------------------
-- Analysis --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- If this looks like magic to you, do not despair! I will try my best to help
-- illuminate how this can possibly work, and where all the explicit state went.
-- Through a series of substitutions (this may feel like high-school algebra
-- class), we will transform the magic-looking do-notation into the explicit
-- state-passing we had previously.

-- Using this function for example:
-- ```purescript                                                             (1)
-- parseFoo :: Parser Int
-- parseFoo = do
--   b <- popByte
--   pure (b + 1)
-- ```

-- The do-notation gets desugared to:
-- ```purescript                                                             (2)
-- parseFoo :: Parser Int
-- parseFoo = popByte >>= \b -> pure (b + 1)
-- ```

-- `(>>=)` is just the infix notation for `bind`. It can be re-written like so:
-- ```purescript                                                             (3)
-- parseFoo :: Parser Int
-- parseFoo = bind popByte (\b -> pure (b + 1))
-- ```

-- Notice how this now looks like the left-hand side of the `Parser` definition
-- for `bind`:
-- ```purescript
-- bind popByte     (\b -> pure (b + 1))
-- bind (Parser p1) f
-- ```
-- . We can then replace this `bind` call with its definition, substituting
-- `(Parser p1)` with `popByte`, and `f` with `(\b -> pure (b + 1))`:
-- ```purescript                                                             (4)
-- parseFoo :: Parser Int
-- parseFoo = Parser \st0 -> do
--  Tuple result st1 <- popByte st0
--  let Parser p2 = (\b -> pure (b + 1)) result
--  p2 st1
-- ```

-- We can make more progress by replacing the call to `pure` with its `Parser`
-- definition, substituting `a` with `(b + 1)`:
-- ```purescript                                                             (5)
-- parseFoo :: Parser Int
-- parseFoo = Parser \st0 -> do
--  Tuple result st1 <- popByte st0
--  let Parser p2 = (\b -> Parser \st -> Right $ Tuple (b + 1) st) result
--  p2 st1
-- ```

-- Beta-reducing the lambda function by replacing `b` with `result`:
-- ```purescript                                                             (6)
-- parseFoo :: Parser Int
-- parseFoo = Parser \st0 -> do
--  Tuple result st1 <- popByte st0
--  let Parser p2 = Parser \st -> Right $ Tuple (result + 1) st
--  p2 st1
-- ```
-- (search terms: `beta-reduction`, `lambda calculus`)

-- Replacing `p2` with its definition:
-- ```purescript                                                             (7)
-- parseFoo :: Parser Int
-- parseFoo = Parser \st0 -> do
--  Tuple result st1 <- popByte st0
--  let Parser p2 = Parser \st -> Right $ Tuple (result + 1) st
--  (\st -> Right $ Tuple (result + 1) st) st1
-- ```

-- Beta-reducing the lambda function by replacing `st` with `st1`:
-- ```purescript                                                             (8)
-- parseFoo :: Parser Int
-- parseFoo = Parser \st0 -> do
--  Tuple result st1 <- popByte st0
--  let Parser p2 = Parser \st -> Right $ Tuple (result + 1) st
--  Right $ Tuple (result + 1) st1
-- ```

-- And we're done! Hopefully this has illuminated how the original function (1)
-- still ultimately has the same behaviour as when the state was being threaded
-- through explicitly; the plumbing is just now abstracted away in the instance
-- definitions.

-- This example showed how it works for a small function with two statements in
-- the do-block; the logic is the same for any number of statements (they all
-- get threaded together with `bind`). I highly recommend going through this
-- substitution process yourself with larger functions. There's nothing that
-- locks in the understanding quite like stepping through it yourself!

-- And don't worry if you don't feel confident with it immediately; it may take
-- weeks or months or years to fully deeply understand, and that's fine! You'll
-- likely get an intuitive working understanding quickly enough to be confident
-- working with monadic parsers in much shorter time. Enjoy!

-- This step had by far the most content in it. The next step addresses
-- improving the errors. The step after that covers some powerful ways to
-- combine parsers!

-- The current state of affairs:
-- 1. Explicit array access             Fixed!
-- 2. Excessive memory use              Fixed!
-- 3. Inconvenient parser combination   Fixed!
-- 4. Basic errors                      We'll address this next!

--------------------------------------------------------------------------------
-- These are the same as in the previous step ----------------------------------
--------------------------------------------------------------------------------

type ParserState = { idx :: Int, arr :: Array Int }

type KV = { key :: String, value :: String }

-- helper function
stringFromCharArray :: Array Int -> Either String String
stringFromCharArray arr = arr
  # traverse (\c -> c # fromCharCode # note ("failed to parse char code: " <> show c))
  <#> fromCharArray
