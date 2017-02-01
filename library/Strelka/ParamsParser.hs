{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParser where

import Strelka.Prelude hiding (maybe, list)
import qualified Strelka.Prelude as A
import qualified Strelka.HTTPAuthorizationParser as D
import qualified Data.Attoparsec.Text as G


newtype Params a =
  Params (ReaderT (Text -> Maybe [Text]) (Except Text) a)
  deriving (Functor, Applicative, Alternative)

value :: Text -> Value a -> Params a
value name value =
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

lenientParams1 :: LenientValue a => Text -> Params a
lenientParams1 name1 =
  value name1 lenientValue

lenientParams2 :: (LenientValue a, LenientValue b) => Text -> Text -> Params (a, b)
lenientParams2 name1 name2 =
  (,) <$>
  lenientParams1 name1 <*>
  lenientParams1 name2

lenientParams3 :: (LenientValue a, LenientValue b, LenientValue c) => Text -> Text -> Text -> Params (a, b, c)
lenientParams3 name1 name2 name3 =
  (,,) <$>
  lenientParams1 name1 <*>
  lenientParams1 name2 <*>
  lenientParams1 name3



newtype Value a =
  Value (ReaderT [Text] (Except Text) a)
  deriving (Functor, Applicative, Alternative)

parser :: G.Parser a -> Value a
parser parser =
  matcher (first fromString . G.parseOnly finishedParser)
  where
    finishedParser =
      parser <* (G.endOfInput <|> fail "Didn't parse the whole data")

matcher :: (Text -> Either Text a) -> Value a
matcher matcher =
  Value (ReaderT (except . join . liftM matcher . head))
  where
    head =
      \case
        x : _ -> Right x
        _ -> Left ("Not a single value is specified")

text :: Value Text
text =
  matcher Right

char :: Value Char
char =
  parser G.anyChar

list :: Value a -> Value [a]
list (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure)))

maybe :: Value a -> Value (Maybe a)
maybe (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure) . listToMaybe))



class LenientValue value where
  lenientValue :: Value value

instance LenientValue a => LenientValue (Maybe a) where
  lenientValue =
    maybe lenientValue

instance LenientValue a => LenientValue [a] where
  lenientValue =
    list lenientValue

instance LenientValue Text where
  lenientValue =
    text

instance LenientValue Char where
  lenientValue =
    char

