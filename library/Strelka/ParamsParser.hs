{-|
DSL for parsing of parameters.
-}
{-# LANGUAGE CPP #-}
module Strelka.ParamsParser
(
  Params,
  param,
  lenientParam,
  -- * Multi-arity lenient param parser helpers
  lenientParams1,
  lenientParams2,
  lenientParams3,
  lenientParams4,
  lenientParams5,
  lenientParams6,
  lenientParams7,
  -- * Value parsers
  Value,
  parser,
  matcher,
  list,
  maybe,
  -- * Implicit lenient value parsers
  LenientValue(..),
)
where

import Strelka.Prelude hiding (maybe, list)
import qualified Strelka.Prelude as A
import qualified Strelka.HTTPAuthorizationParser as D
import qualified Data.Attoparsec.Text as G
import qualified Data.Text as E
import qualified Attoparsec.Data.Implicit as B


newtype Params a =
  Params (ReaderT (Text -> Maybe [Text]) (Except Text) a)
  deriving (Functor, Applicative, Alternative)

-- | Parse a param by its name using an explicit parser.
{-# INLINABLE param #-}
param :: Text -> Value a -> Params a
param name value =
  Params (ReaderT onLookup)
  where
    onLookup lookup =
      A.maybe notFound found (lookup name)
      where
        notFound =
          (except . Left) ("Parameter \"" <> name <> "\" not found")
        found input =
          case value of
            Value (ReaderT onInput) -> withExcept updateError (onInput input)
          where
            updateError x =
              "Parameter \"" <> name <> "\" values parsing failure: " <> x

-- | Parse a param by its name using the implicit lenient parser.
{-# INLINE lenientParam #-}
lenientParam :: LenientValue a => Text -> Params a
lenientParam name =
  param name lenientValue

-- | Same as 'lenientParam'.
{-# INLINE lenientParams1 #-}
lenientParams1 :: LenientValue a => Text -> Params a
lenientParams1 =
  lenientParam

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams2 #-}
lenientParams2 :: (LenientValue a, LenientValue b) => Text -> Text -> Params (a, b)
lenientParams2 name1 name2 =
  (,) <$>
  lenientParam name1 <*>
  lenientParam name2

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams3 #-}
lenientParams3 :: (LenientValue a, LenientValue b, LenientValue c) => Text -> Text -> Text -> Params (a, b, c)
lenientParams3 name1 name2 name3 =
  (,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams4 #-}
lenientParams4 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d) => Text -> Text -> Text -> Text -> Params (a, b, c, d)
lenientParams4 name1 name2 name3 name4 =
  (,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams5 #-}
lenientParams5 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e) => Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e)
lenientParams5 name1 name2 name3 name4 name5 =
  (,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams6 #-}
lenientParams6 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e, LenientValue f) => Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f)
lenientParams6 name1 name2 name3 name4 name5 name6 =
  (,,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5 <*>
  lenientParam name6

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams7 #-}
lenientParams7 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e, LenientValue f, LenientValue g) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f, g)
lenientParams7 name1 name2 name3 name4 name5 name6 name7 =
  (,,,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5 <*>
  lenientParam name6 <*>
  lenientParam name7


newtype Value a =
  Value (ReaderT [Text] (Except Text) a)
  deriving (Functor, Applicative, Alternative)

{-# INLINE parser #-}
parser :: G.Parser a -> Value a
parser parser =
  matcher (first fromString . G.parseOnly finishedParser)
  where
    finishedParser =
      parser <* (G.endOfInput <|> fail "Didn't parse the whole data")

{-# INLINE matcher #-}
matcher :: (Text -> Either Text a) -> Value a
matcher matcher =
  Value (ReaderT (except . join . liftM matcher . head))
  where
    head =
      \case
        x : _ -> Right x
        _ -> Left ("Not a single value is specified")

{-# INLINE text #-}
text :: Value Text
text =
  matcher Right

{-# INLINE string #-}
string :: Value String
string =
  matcher (Right . E.unpack)

{-# INLINE list #-}
list :: Value a -> Value [a]
list (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure)))

{-# INLINE maybe #-}
maybe :: Value a -> Value (Maybe a)
maybe (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure) . listToMaybe))


{-|
Provides a default lenient value parser.
-}
class LenientValue value where
  lenientValue :: Value value

instance LenientValue a => LenientValue (Maybe a) where
  lenientValue =
    maybe lenientValue

instance LenientValue a => LenientValue [a] where
  lenientValue =
    list lenientValue


-- * Generated LenientValue instances
-------------------------

#define INSTANCE1(TYPE, FUNCTION) instance LenientValue TYPE where {{-# INLINE lenientValue #-}; lenientValue = FUNCTION;}
#define INSTANCE2(TYPE) INSTANCE1(TYPE, parser B.lenientParser)

INSTANCE1(Text, matcher Right)
INSTANCE1(String, string)
-- | Encodes the input using UTF8.
INSTANCE2(ByteString)
INSTANCE2(Char)
INSTANCE2(Bool)
INSTANCE2(Integer)
INSTANCE2(Int)
INSTANCE2(Int8)
INSTANCE2(Int16)
INSTANCE2(Int32)
INSTANCE2(Int64)
INSTANCE2(Word)
INSTANCE2(Word8)
INSTANCE2(Word16)
INSTANCE2(Word32)
INSTANCE2(Word64)
INSTANCE2(Double)
INSTANCE2(Scientific)
INSTANCE2(TimeOfDay)
INSTANCE2(Day)
INSTANCE2(TimeZone)
INSTANCE2(UTCTime)

#undef INSTANCE1
#undef INSTANCE2

