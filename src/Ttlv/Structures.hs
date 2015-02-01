{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Ttlv.Structures where

import Ttlv.Tag
import Ttlv.Data
import Control.Lens
import qualified Data.List as L
import qualified Ttlv.Enum as TEnum

import Test.Hspec

newtype TtlvParser a = TtlvParser { runTtlvParser :: (a -> Either [String] a) }

-- instance Monad TtlvParser where
--   return  = TtlvParser
--   -- m a -> (a -> m b) -> m b
--   x >>= y = x <+> (return . y)

-- | Ttlv parser combinator
infixl 9 <+>
(<+>) :: TtlvParser Ttlv -> TtlvParser Ttlv -> TtlvParser Ttlv
x <+> y = TtlvParser $ \t ->
  case runTtlvParser x t of
    Right t' -> runTtlvParser y t'
    Left e -> Left $ "failed combinator" : e

-- | run one parser or the other
either :: TtlvParser a -> TtlvParser a -> TtlvParser a
either x y = TtlvParser $ \t -> case runTtlvParser x t of
  x'@(Right _) -> x'
  Left _ -> case runTtlvParser y t of
    y'@(Right _) -> y'
    Left e -> Left $ "neither matched" : e

(<|>) :: TtlvParser a -> TtlvParser a -> TtlvParser a
(<|>) = Ttlv.Structures.either


-- | on: Ttlv
-- | look at Ttlv and see if it matches; do not remove matched data
check :: String -> (Ttlv -> Bool) -> TtlvParser Ttlv
check msg f = TtlvParser $ \t -> if f t
                                 then Right t
                                 else Left ["failed check: " ++ msg]

-- | take struct `t` and run parser `p` on it if exists, removing
-- | the struct if it parses without error; otherwise return error
struct :: TtlvParser Ttlv -> Ttlv -> TtlvParser Ttlv
struct p t = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == length s
                then runTtlvParser p (head s)
                else Left ["couldn't find structure"]
    Nothing -> Left ["not a structure"]

-- | check if struct contains a specific tag
struct1 :: (Ttlv -> Bool) -> TtlvParser Ttlv
struct1 fn = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == (length $ filter fn s)
                then Right t
                else Left ["not enough/too many"]
    Nothing -> Left ["not a structure"]

-- | on: Structure
-- | return: The Ttlv for `tag`
-- | find a tag from structure
find :: TtlvTag -> TtlvParser Ttlv
find tag = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    Nothing -> Left ["not a structure"]
    Just s -> case L.find (\t' -> getTtlvTag t' == tag) s of
      Just t' -> Right t'
      Nothing -> Left ["unable to find tag " ++ show tag]

-- | on: Ttlv
-- | check current Ttlv tag
tag :: TtlvTag -> TtlvParser Ttlv
tag tag' = TtlvParser $ \t -> if getTtlvTag t == tag'
                              then Right t
                              else Left ["current tag not " ++ show tag']

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
integer :: TtlvParser Ttlv
integer = TtlvParser $ \t -> case getTtlvData t of
  TtlvInt _ -> Right t
  otherwise -> Left ["not an integer"]

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

-- Objects

-- | on: Structure
-- | Check Attribute object
attribute :: String -> TtlvParser Ttlv -> TtlvParser Ttlv
attribute name vf = TtlvParser $ \t ->
  let n  = apply    TtlvAttributeName (stringEq name)
      v  = apply    TtlvAttributeValue vf
      i  = optional TtlvAttributeIndex integer
      fn = tag TtlvAttribute <+> n <+> v <+> i
  in runTtlvParser fn t

-- | on: Structure
-- | Check Credential object
credential :: TtlvParser Ttlv
credential = TtlvParser $ \t ->
  let typ = apply TtlvCredentialType tenum
      val = apply TtlvCredentialValue credentialValue
  in runTtlvParser (typ <+> val) t

credentialValue :: TtlvParser Ttlv
credentialValue = apply TtlvUsername string <+> apply TtlvPassword string

keyBlock :: TtlvParser Ttlv
keyBlock = apply    TtlvKeyFormatType tenum <+>
           optional TtlvKeyCompressionType tenum <+>
           apply    TtlvKeyValue keyValue <+>
           optional TtlvCryptographicAlgorithm tenum <+> -- FIXME
           optional TtlvCryptographicLength integer <+>  -- FIXME
           optional TtlvKeyWrappingData tstruct -- FIXME
           
keyValue :: TtlvParser Ttlv
keyValue = apply    TtlvKeyMaterial ok <+>
           optional TtlvAttribute ok -- FIXME

-- Managed Objects


-- Testing
xx = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-hi")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 0)
                                        , Ttlv TtlvAttributeValue (TtlvString "hello world") ]))
yy = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-yo")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 0)
                                        , Ttlv TtlvAttributeValue (TtlvString "sticker") ]))

zz = Ttlv TtlvCredential (TtlvStructure [ Ttlv TtlvCredentialType (TtlvEnum $ TEnum.fromTtlvEnum TEnum.UsernameAndPassword)
                                        , Ttlv TtlvCredentialValue (TtlvStructure [ Ttlv TtlvUsername (TtlvString "aaron")
                                                                                  , Ttlv TtlvPassword (TtlvString "password") ])])

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
    it "should fail bad matches" $ do
      let x = apply TtlvAttributeName (stringEq "abc")
      runTtlvParser x xx `shouldBe` Left ["mismatch in expected value"]

    describe "Objects" $ do
      describe "Attribute" $ do
        it "valid" $ do
          runTtlvParser (attribute "x-hi" string) xx `shouldBe` Right xx
        it "unexpected attribute" $ do
          runTtlvParser (attribute "x-hi" integer) xx `shouldBe` Left ["failed combinator", "not an integer"]
      describe "Credential" $ do
        it "valid" $ do
          runTtlvParser credential zz `shouldBe` Right zz
