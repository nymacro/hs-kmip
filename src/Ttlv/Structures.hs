{-# LANGUAGE TemplateHaskell #-}
module Ttlv.Structures where

import Ttlv.Tag
import Ttlv.Data
import Control.Lens
import qualified Data.List as L

import Test.Hspec

getAttributeName :: Ttlv -> TtlvData
getAttributeName t = head $ map getTtlvData $ filter (\a -> getTtlvTag a == TtlvAttributeName) $ getTtlvData t ^. _TtlvStructure

getAttributeIndex :: Ttlv -> TtlvData
getAttributeIndex t = head $ map getTtlvData $ filter (\a -> getTtlvTag a == TtlvAttributeIndex) $ getTtlvData t ^. _TtlvStructure

getAttributeValue :: Ttlv -> TtlvData
getAttributeValue t = head $ map getTtlvData $ filter (\a -> getTtlvTag a == TtlvAttributeValue) $ getTtlvData t ^. _TtlvStructure

getCredentialType :: Ttlv -> TtlvData
getCredentialType t = head $ map getTtlvData $ filter (\a -> getTtlvTag a == TtlvCredentialType) $ getTtlvData t ^. _TtlvStructure

getCredentialValue :: Ttlv -> TtlvData
getCredentialValue t = head $ map getTtlvData $ filter (\a -> getTtlvTag a == TtlvCredentialValue) $ getTtlvData t ^. _TtlvStructure

-- check :: (Ttlv -> Either String Ttlv) -> Ttlv -> Either String Ttlv
-- check f ttlv = f ttlv

-- tagType :: TtlvTag -> (TtlvData -> Bool) -> Ttlv -> Either String Ttlv
-- tagType tag typ ttlv = if tag == getTtlvTag ttlv
--                        then if typ $ getTtlvData ttlv
--                             then Right ttlv
--                             else Left "type mismatch"
--                        else Left "tag mismatch"

-- checkTag :: TtlvTag -> Ttlv -> Either String Ttlv
-- checkTag t tt = if getTtlvTag tt == t
--                 then Right tt
--                 else Left $ "couldn't find tag " ++ show t


-- -- FIXME
-- structure :: Ttlv -> Either String Ttlv
-- structure tt = let s = (getTtlvData tt) ^? _TtlvStructure
--                in case s of
--                  (Just st) -> Right tt
--                  otherwise -> Left "not a structure"

-- check1 :: (Ttlv -> Either String Ttlv) -> Ttlv -> Either String Ttlv
-- check1 f tt = structure tt >>= \ts -> let matches = filter (isRight . f) $ (getTtlvData ts) ^. _TtlvStructure
--                                      in if length matches == 1
--                                         then Right $ head matches
--                                         else Left "too many/not any"
--   where isRight (Right _) = True
--         isRight (Left _) = False

-- test :: IO ()
-- test = do
--   hspec $ do
--     describe "Attribute" $ do
--       it "should decode attributes" $ do
--         let attribute = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-hi")
--                                                            , Ttlv TtlvAttributeIndex (TtlvInt 0)
--                                                            , Ttlv TtlvAttributeValue (TtlvString "hello world") ]))
--         getAttributeValue attribute `shouldBe` (TtlvString "hello world")
--         getAttributeName attribute `shouldBe` (TtlvString "x-hi")
--         getAttributeIndex attribute `shouldBe` (TtlvInt 0)

newtype TtlvParser a = TtlvParser { runTtlvParser :: (a -> Either String a) }

-- | Ttlv parser combinator
(<+>) :: TtlvParser Ttlv -> TtlvParser Ttlv -> TtlvParser Ttlv
x <+> y = TtlvParser $ \t ->
  case runTtlvParser x t of
    Right t' -> runTtlvParser y t'
    Left _ -> Left "failed combinator"

-- | run one parser or the other
either :: TtlvParser a -> TtlvParser a -> TtlvParser a
either x y = TtlvParser $ \t -> case runTtlvParser x t of
  x'@(Right _) -> x'
  Left _ -> case runTtlvParser y t of
    y'@(Right _) -> y'
    Left _ -> Left "neither matched"

(<|>) :: TtlvParser a -> TtlvParser a -> TtlvParser a
(<|>) = Ttlv.Structures.either


-- | on: Ttlv
-- | look at Ttlv and see if it matches; do not remove matched data
check :: String -> (Ttlv -> Bool) -> TtlvParser Ttlv
check msg f = TtlvParser $ \t -> if f t
                                 then Right t
                                 else Left $ "failed check: " ++ msg

-- | take struct `t` and run parser `p` on it if exists, removing
-- | the struct if it parses without error; otherwise return error
struct :: TtlvParser Ttlv -> Ttlv -> TtlvParser Ttlv
struct p t = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == length s
                then runTtlvParser p (head s)
                else Left "couldn't find structure"
    Nothing -> Left "not a structure"

-- | check if struct contains a specific tag
struct1 :: (Ttlv -> Bool) -> TtlvParser Ttlv
struct1 fn = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == (length $ filter fn s)
                then Right t
                else Left "not enough/too many"
    Nothing -> Left "not a structure"

-- | on: Structure
-- | return: The Ttlv for `tag`
-- | find a tag from structure
find :: TtlvTag -> TtlvParser Ttlv
find tag = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    Nothing -> Left "not a structure"
    Just s -> case L.find (\t' -> getTtlvTag t' == tag) s of
      Just t' -> Right t'
      Nothing -> Left $ "unable to find tag " ++ show tag

-- | on: Ttlv
-- | check current Ttlv tag
tag :: TtlvTag -> TtlvParser Ttlv
tag tag' = TtlvParser $ \t -> if getTtlvTag t == tag'
                              then Right t
                              else Left $ "current tag not " ++ show tag'

string :: TtlvParser Ttlv
string = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  otherwise -> Left "not a string"

stringEq :: String -> TtlvParser Ttlv
stringEq s = TtlvParser $ \t -> case getTtlvData t of
  TtlvString x -> if x == s
                  then Right t
                  else Left "mismatch in expected value"
  otherwise -> Left "not a string"

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

-- # combinatorial interface which removes elements from list which are to match
-- # ensure that all elements from list are consumed at the end of match (ie. [])
-- # allow conditional matching & access to full Ttlv tree for cross-checks (if required)


xx = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-hi")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 0)
                                        , Ttlv TtlvAttributeValue (TtlvString "hello world") ]))
yy = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-yo")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 0)
                                        , Ttlv TtlvAttributeValue (TtlvString "sticker") ]))

test :: IO ()
test = hspec $ do
  describe "Validator" $ do
    it "should sub-apply validators" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
      runTtlvParser x xx `shouldBe` Right xx
    it "should allow chaining of validators" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
      let y = apply TtlvAttributeValue ok
      runTtlvParser (x <+> y) xx `shouldBe` Right xx
    it "should allow chaining of validators (alternation)" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
          y = apply TtlvAttributeName (stringEq "x-yo")
          p = x <|> y
      runTtlvParser p xx `shouldBe` Right xx
      runTtlvParser p yy `shouldBe` Right yy


testParser :: IO ()
testParser = do
  let x = apply TtlvAttributeName (stringEq "x-hi")
      y = apply TtlvAttributeValue ok
      z = optional TtlvAttributeIndex ok -- (stringEq "alsjkd")
  -- let x = struct1 (\t -> getTtlvTag t == TtlvAttributeName)
  -- let y = struct1 (\t -> getTtlvTag t == TtlvAttributeValue)
  putStrLn $ show $ runTtlvParser (x <+> y <+> z) xx
