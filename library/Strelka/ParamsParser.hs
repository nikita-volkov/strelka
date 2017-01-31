{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParser where

import Strelka.Prelude
import qualified Strelka.HTTPAuthorizationParser as D
import qualified Data.Attoparsec.Text as G


newtype Params a =
  Params (ReaderT (Text -> Maybe [Text]) (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

value :: Text -> Value a -> Params a
value name value =
  valueMaybe name value >>= \case
    Just x -> return x
    Nothing -> throwError ("No value specified for the parameter \"" <> name <> "\"")

valueList :: Text -> Value a -> Params [a]
valueList name value =
  Strelka.ParamsParser.lookup name >>= parseValues value
  where
    parseValues (Value parser) inputs =
      Params (lift (traverse (except . either (Left . updateError) Right . runExcept . runReaderT parser) inputs))
      where
        updateError x =
          "Parameter \"" <> name <> "\" parsing failure: " <> x

valueMaybe :: Text -> Value a -> Params (Maybe a)
valueMaybe name =
  fmap listToMaybe . valueList name

lookup :: Text -> Params [Text]
lookup name =
  Params (ReaderT (\lookup -> maybe notFound return (lookup name)))
  where
    notFound =
      throwError ("Parameter \"" <> name <> "\" not found")


newtype Value a =
  Value (ReaderT Text (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

parser :: G.Parser a -> Value a
parser parser =
  matcher (first fromString . G.parseOnly finishedParser)
  where
    finishedParser =
      parser <* (G.endOfInput <|> fail "Didn't parse the whole data")

matcher :: (Text -> Either Text a) -> Value a
matcher matcher =
  Value (ReaderT (except . matcher))

text :: Value Text
text =
  Value (ReaderT pure)

char :: Value Char
char =
  parser G.anyChar



class LenientValue value where
  lenientValue :: Value value

instance LenientValue Text where
  lenientValue =
    text

instance LenientValue Char where
  lenientValue =
    char


class LenientParams fn where
  lenientParams :: fn

instance LenientValue a => LenientParams (Text -> Params a) where
  lenientParams name1 =
    value name1 lenientValue

instance (LenientValue a, LenientValue b) => LenientParams (Text -> Text -> Params (a, b)) where
  lenientParams name1 name2 =
    (,) <$> lenientParams name1 <*> lenientParams name2

instance (LenientValue a, LenientValue b, LenientValue c) => LenientParams (Text -> Text -> Text -> Params (a, b, c)) where
  lenientParams name1 name2 name3 =
    (,,) <$> lenientParams name1 <*> lenientParams name2 <*> lenientParams name3

