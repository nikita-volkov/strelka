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

class DefaultParams keys values where
  defaultParams :: keys -> Params values

instance DefaultValue a => DefaultParams Text a where
  defaultParams key =
    value key defaultValue

instance DefaultValue a => DefaultParams Text [a] where
  defaultParams key =
    valueList key defaultValue

instance (DefaultParams keys1 values1, DefaultParams keys2 values2) => DefaultParams (keys1, keys2) (values1, values2) where
  defaultParams (keys1, keys2) =
    (,) <$> defaultParams keys1 <*> defaultParams keys2

instance (DefaultParams keys1 values1, DefaultParams keys2 values2, DefaultParams keys3 values3) => DefaultParams (keys1, keys2, keys3) (values1, values2, values3) where
  defaultParams (keys1, keys2, keys3) =
    (,,) <$> defaultParams keys1 <*> defaultParams keys2 <*> defaultParams keys3


