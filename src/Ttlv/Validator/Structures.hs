-- KMIP Structure Validation
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Ttlv.Validator.Structures ( TtlvParser'(..)
                                 , TtlvParser(..)
                                 , (<+>)
                                 , check
                                 , find
                                 , many
                                 , many1
                                 , tag
                                 , apply
                                 , optional
                                 , ok
                                 , nok
                                 , get
                                 , under
                                 , with
                                 , okIf
                                 , nokIf
                                 ) where

import           Control.Lens ((^?))
import           Data.Either
import           Ttlv.Data
import           Ttlv.Tag
import           Ttlv.Lens

import           Control.Applicative hiding (many, optional)

-- TODO fail on input which has not been validated.
--      `TtlvParser'`s b could be (* Ttlv, Ttlv), one being the full structure,
--       the other containing unvalidated input.

newtype TtlvParser' a b = TtlvParser { runTtlvParser :: a -> Either [String] b }
type TtlvParser a = TtlvParser' a a

instance Functor (TtlvParser' a) where
  -- fmap fn (TtlvParser x) = TtlvParser ((\b2 b3 -> fmap fn (b2 ((\b1 -> b1) b3))) x)
  -- simplified:
  fmap fn (TtlvParser x) = TtlvParser ((\a b -> fn <$> a b) x)

instance Applicative (TtlvParser' a) where
  pure = return
  -- f (a -> b) -> f a -> f b
  a <*> b = do
    f <- a
    x <- b
    return (f x)

instance Monad (TtlvParser' a) where
  return x = TtlvParser (\_ -> Right x)
  x >>= y = TtlvParser $ \t ->
    case runTtlvParser x t of
      Right t' -> runTtlvParser (y t') t -- is this right?
      Left e   -> Left $ "failed combinator" : e

-- | Ttlv parser combinator
infixl 9 <+>
(<+>) :: TtlvParser a -> TtlvParser a -> TtlvParser a
x <+> y = TtlvParser $ \t ->
  case runTtlvParser x t of
    Right t' -> runTtlvParser y t'
    Left e   -> Left $ "failed combinator" : e

-- | Alternative instance for TtlvParser
instance Alternative (TtlvParser' a) where
  x <|> y = TtlvParser $ \t -> case runTtlvParser x t of
    x'@(Right _) -> x'
    Left e -> case runTtlvParser y t of
      y'@(Right _) -> y'
      Left e' -> Left ["neither matched: " ++ concat e ++ " | " ++ concat e']
  empty = TtlvParser $ \_ -> Left ["empty"]

-- | on: Ttlv
--   look at Ttlv and see if it matches
check :: String -> (Ttlv -> Bool) -> TtlvParser Ttlv
check msg f = TtlvParser $ \t -> if f t
                                 then Right t
                                 else Left ["failed check: " ++ msg]

-- | on: Structure
--   return: The Ttlv for `tag`
--   find a tag from structure
find :: TtlvTag -> TtlvParser Ttlv
find tag = TtlvParser $ \t ->
  case getTtlvData t ^? _TtlvStructure of
    Nothing -> Left ["not a structure " ++ show tag]
    Just s -> case filter (\t' -> getTtlvTag t' == Tag tag) s of
      [] -> Left ["unable to find tag " ++ show tag]
      xs -> if length xs /= 1
            then Left ["too many/too few " ++ show tag]
            else Right $ head xs

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft (Right _) = undefined

fromRight :: Either a b -> b
fromRight (Left _) = undefined
fromRight (Right x) = x

-- | on: Ttlv
--   zero-many
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
--   one-many
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
--   check current Ttlv tag
tag :: TtlvTag -> TtlvParser Ttlv
tag tag' = TtlvParser $ \t -> if getTtlvTag t == Tag tag'
                              then Right t
                              else Left ["current tag not " ++ show tag' ++ " but " ++ show (getTtlvTag t)]

-- | on: Structure
--   run `parser` on `tag`, returning original Ttlv
apply :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
apply tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left y  -> Left y

-- | on: Structure
--   like apply, but will run `parser` only if tag exists. Will pass
--   by default.
optional :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
optional tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left _  -> Right t

-- | identity for TtlvParser
ok :: TtlvParser Ttlv
ok = TtlvParser $ \t -> Right t

-- | Error with message
nok :: String -> TtlvParser Ttlv
nok msg = TtlvParser $ \t -> Left [msg]

-- | extract data from ttlv
--   e.g.
--     do { x <- get T.AttributeName ; ... }
get :: TtlvTag -> TtlvParser' Ttlv TtlvData
get tag = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> Right $ getTtlvData x
    Left y  -> Left y

-- | run TtlvParser on tag in Ttlv
under :: TtlvTag -> TtlvParser Ttlv
under tag = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> Right x
    Left y  -> Left y

-- helper functions

-- | take a Lens getter t and check whether v matches and apply function f to the
--   value of v
--   e.g.
--     with _TtlvInt major $ nokIf (> 0) "failed"
with t v f = case v ^? t of
  Just x  -> f x
  Nothing -> nok "failed"

-- | nok with message `msg` if value `v` applied to condition `c`
nokIf :: (a -> Bool) -> String -> a -> TtlvParser Ttlv
nokIf c msg v = if c v
                then nok msg
                else ok

-- | inverse of nokIf
okIf :: (a -> Bool) -> String -> a -> TtlvParser Ttlv
okIf c msg v = if c v
               then ok
               else nok msg
