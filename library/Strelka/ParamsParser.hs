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

instance DefaultValue Char where
  defaultValue =
    char


defaultParams1 :: DefaultValue a => Text -> Params a
defaultParams1 name1 =
  value name1 defaultValue

defaultParams2 :: (DefaultValue a, DefaultValue b) => Text -> Text -> Params (a, b)
defaultParams2 name1 name2 =
  (,) <$> defaultParams1 name1 <*> defaultParams1 name2

defaultParams3 :: (DefaultValue a, DefaultValue b, DefaultValue c) => Text -> Text -> Text -> Params (a, b, c)
defaultParams3 name1 name2 name3 =
  (,,) <$> defaultParams1 name1 <*> defaultParams1 name2 <*> defaultParams1 name3

  
