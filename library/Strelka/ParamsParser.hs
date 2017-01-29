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
value name (Value parser) =
  undefined

valueList :: Text -> Value a -> Params [a]
valueList name (Value parser) =
  undefined

valueMaybe :: Text -> Value a -> Params (Maybe a)
valueMaybe =
  undefined


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




class DefaultValue value where
  defaultValue :: Value value

instance DefaultValue Text where
  defaultValue =
    text

type family KeysByValues values where
  KeysByValues (a, b, c, d, e, f, g) = (KeysByValues a, KeysByValues b, KeysByValues c, KeysByValues d, KeysByValues e, KeysByValues f, KeysByValues g)
  KeysByValues (a, b, c, d, e, f) = (KeysByValues a, KeysByValues b, KeysByValues c, KeysByValues d, KeysByValues e, KeysByValues f)
  KeysByValues (a, b, c, d, e) = (KeysByValues a, KeysByValues b, KeysByValues c, KeysByValues d, KeysByValues e)
  KeysByValues (a, b, c, d) = (KeysByValues a, KeysByValues b, KeysByValues c, KeysByValues d)
  KeysByValues (a, b, c) = (KeysByValues a, KeysByValues b, KeysByValues c)
  KeysByValues (a, b) = (KeysByValues a, KeysByValues b)
  KeysByValues a = Text


class DefaultParams values where
  defaultParams :: KeysByValues values -> Params values

instance DefaultValue a => DefaultParams a where
  defaultParams key =
    value key defaultValue

instance DefaultValue a => DefaultParams [a] where
  defaultParams key =
    valueList key defaultValue

instance (DefaultParams values1, DefaultParams values2) => DefaultParams (values1, values2) where
  defaultParams (keys1, keys2) =
    (,) <$> defaultParams keys1 <*> defaultParams keys2


