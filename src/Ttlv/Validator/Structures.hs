-- KMIP Structure Validation
{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleInstances #-}
module Ttlv.Validator.Structures where

import Ttlv.Tag
import Ttlv.Data
import Control.Lens
import qualified Data.List as L
import Data.Either

import Control.Applicative

-- TODO fail on input which has not been validated.
--      `TtlvParser'`s b could be (* Ttlv, Ttlv), one being the full structure,
--       the other containing unvalidated input.

newtype TtlvParser' a b = TtlvParser { runTtlvParser :: a -> Either [String] b }
                        deriving (Functor)
type TtlvParser a = TtlvParser' a a

-- derived instance looks like:
-- instance Functor (TtlvParser' a) where
--   fmap fn (TtlvParser x) = TtlvParser ((\b2 b3 -> fmap fn (b2 ((\b1 -> b1) b3))) x)
--   -- simplified:
--   fmap fn (TtlvParser x) = TtlvParser ((\a b -> fmap fn (a $ b)) x)

instance Applicative (TtlvParser' a) where
  pure x = TtlvParser (\_ -> Right x)
  -- f (a -> b) -> f a -> f b
  -- (TtlvParser x) <*> (TtlvParser y) = TtlvParser $ \t -> (runTtlvParser (x t)) . (runTtlvParser y)
  x <*> y = undefined

instance Monad (TtlvParser' a) where
  return x = TtlvParser (\_ -> Right x)
  -- m a -> (a -> m b) -> m b
  x >>= y = TtlvParser $ \t ->
    case runTtlvParser x t of
      Right t' -> runTtlvParser (y t') t -- is this right?
      Left e   -> Left $ "failed combinator" : e

-- | Ttlv parser combinator
infixl 9 <+>
(<+>) :: TtlvParser Ttlv -> TtlvParser Ttlv -> TtlvParser Ttlv
x <+> y = TtlvParser $ \t ->
  case runTtlvParser x t of
    Right t' -> runTtlvParser y t'
    Left e   -> Left $ "failed combinator" : e

-- | run one parser or the other
either :: TtlvParser a -> TtlvParser a -> TtlvParser a
either x y = TtlvParser $ \t -> case runTtlvParser x t of
  x'@(Right _) -> x'
  Left e -> case runTtlvParser y t of
    y'@(Right _) -> y'
    Left e' -> Left ["neither matched: " ++ concat e ++ " | " ++ concat e']

infixl 8 <|>
(<|>) :: TtlvParser a -> TtlvParser a -> TtlvParser a
(<|>) = Ttlv.Validator.Structures.either


-- | on: Ttlv
-- | look at Ttlv and see if it matches; do not remove matched data
check :: String -> (Ttlv -> Bool) -> TtlvParser Ttlv
check msg f = TtlvParser $ \t -> if f t
                                 then Right t
                                 else Left ["failed check: " ++ msg]

-- | on: Structure
-- | return: The Ttlv for `tag`
-- | find a tag from structure
find :: TtlvTag -> TtlvParser Ttlv
find tag = TtlvParser $ \t ->
  case getTtlvData t ^? _TtlvStructure of
    Nothing -> Left ["not a structure " ++ show tag]
    Just s -> case filter (\t' -> getTtlvTag t' == Tag tag) s of
      [] -> Left ["unable to find tag " ++ show tag]
      xs -> if length xs /= 1
            then Left ["too many/too few " ++ show tag]
            else Right $ head xs

fromLeft (Left x) = x
fromLeft (Right _) = undefined

fromRight (Left _) = undefined
fromRight (Right x) = x

-- | on: Ttlv
-- | zero-many
many :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
many tag v = TtlvParser $ \t ->
  case getTtlvData t ^? _TtlvStructure of
    Nothing -> Left ["not a structure"]
    Just s -> case filter (\t' -> getTtlvTag t' == Tag tag) s of
      [] -> Right t
      xs -> let eithers = map (runTtlvParser v) xs
                errors = filter isLeft eithers
            in if all isRight eithers
               then Right t
               else Left $ ("not everything matched for tag " ++ show tag) : concatMap fromLeft errors

-- | on: Ttlv
-- | one-many
many1 :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
many1 tag v = TtlvParser $ \t ->
  case getTtlvData t ^? _TtlvStructure of
    Nothing -> Left ["not a structure" ++ show tag]
    Just s -> case filter (\t' -> getTtlvTag t' == Tag tag) s of
      [] -> Left ["unable to find tag " ++ show tag]
      xs -> let eithers = map (runTtlvParser v) xs
                errors = filter isLeft eithers
            in if all isRight eithers
               then Right t
               else Left $ ("not everything matched for tag " ++ show tag) : concatMap fromLeft errors

-- | on: Ttlv
-- | check current Ttlv tag
tag :: TtlvTag -> TtlvParser Ttlv
tag tag' = TtlvParser $ \t -> if getTtlvTag t == Tag tag'
                              then Right t
                              else Left ["current tag not " ++ show tag' ++ " but " ++ show (getTtlvTag t)]

-- | on: Ttlv
-- | check Ttlv data type
string :: TtlvParser Ttlv
string = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  otherwise -> Left ["not a string"]

-- | on: Ttlv
-- | check Ttlv data type and value
stringEq :: String -> TtlvParser Ttlv
stringEq s = TtlvParser $ \t -> case getTtlvData t of
  TtlvString x -> if x == s
                  then Right t
                  else Left ["mismatch in expected value"]
  otherwise -> Left ["not a string"]

-- | on: Ttlv
-- | check Ttlv data type
tstruct :: TtlvParser Ttlv
tstruct = TtlvParser $ \t -> case getTtlvData t of
  TtlvStructure _ -> Right t
  otherwise -> Left ["not a structure"]

-- | on: Ttlv
-- | check Ttlv data type
tenum :: TtlvParser Ttlv
tenum = TtlvParser $ \t -> case getTtlvData t of
  TtlvEnum e -> Right t
  otherwise  -> Left ["not an enum"]

tbytestring :: TtlvParser Ttlv
tbytestring = TtlvParser $ \t -> case getTtlvData t of
  TtlvByteString e -> Right t
  otherwise  -> Left ["not a byte-string"]

tstring :: TtlvParser Ttlv
tstring = TtlvParser $ \t -> case getTtlvData t of
  TtlvString e -> Right t
  otherwise  -> Left ["not a string"]

tbigint :: TtlvParser Ttlv
tbigint = TtlvParser $ \t -> case getTtlvData t of
  TtlvBigInt e -> Right t
  otherwise  -> Left ["not a big int"]

tint :: TtlvParser Ttlv
tint = TtlvParser $ \t -> case getTtlvData t of
  TtlvInt e -> Right t
  otherwise  -> Left ["not an int"]

tlong :: TtlvParser Ttlv
tlong = TtlvParser $ \t -> case getTtlvData t of
  TtlvLongInt e -> Right t
  otherwise  -> Left ["not an int"]

tinterval :: TtlvParser Ttlv
tinterval = TtlvParser $ \t -> case getTtlvData t of
  TtlvInterval e -> Right t
  otherwise  -> Left ["not an int"]

tdatetime :: TtlvParser Ttlv
tdatetime = TtlvParser $ \t -> case getTtlvData t of
  TtlvDateTime e -> Right t
  otherwise  -> Left ["not an int"]

tbool :: TtlvParser Ttlv
tbool = TtlvParser $ \t -> case getTtlvData t of
  TtlvBool e -> Right t
  otherwise  -> Left ["not a bool"]

-- | on: Structure
-- | run `parser` on `tag`, returning original Ttlv
apply :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
apply tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left y  -> Left y

-- | on: Structure
-- | like apply, but will run `parser` only if tag exists. Will pass
-- | by default.
optional :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
optional tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left _  -> Right t

-- | on: *
-- | identity for TtlvParser
ok :: TtlvParser Ttlv
ok = TtlvParser $ \t -> Right t

-- | on: *
-- | return an error
nok :: String -> TtlvParser Ttlv
nok msg = TtlvParser $ \t -> Left [msg]

-- | extract data from ttlv
get :: TtlvTag -> TtlvParser' Ttlv TtlvData
get tag = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> Right $ getTtlvData x
    Left y  -> Left y