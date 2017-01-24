{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParser where

import Strelka.Prelude
import Strelka.Core.Model
import qualified Strelka.HTTPAuthorizationParser as D
import qualified Data.Attoparsec.ByteString.Char8 as F
import qualified Data.Attoparsec.Zepto as E
import qualified Data.Attoparsec.Text as G
import qualified Data.Text.Encoding as I
import qualified Data.Text.Encoding.Error as J


newtype Params a =
  Params (ReaderT (HashMap Text ParamValue) (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

newtype Value a =
  Value (ReaderT ByteString (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

liftEitherToValue :: Either Text a -> Value a
liftEitherToValue =
  Value . lift . except

matchValueBytes :: (ByteString -> Either Text a) -> Value a
matchValueBytes matcher =
  Value (ReaderT (except . matcher))

matchValueText :: (Text -> Either Text a) -> Value a
matchValueText matcher =
  matchValueBytes bytesMatcher
  where
    bytesMatcher =
      (=<<) matcher . first exceptionMapper . I.decodeUtf8'
      where
        exceptionMapper (J.DecodeError message _) =
          fromString ("UTF8 decoding error: " <> message)

parseValueBytes :: F.Parser a -> Value a
parseValueBytes parser =
  matchValueBytes (first fromString . F.parseOnly parser)

parseValueBytesWithZepto :: E.Parser a -> Value a
parseValueBytesWithZepto parser =
  matchValueBytes (first fromString . E.parse parser)

{-|
Decode the value to 'Text' and then run a text Parser on it.
-}
parseValueText :: G.Parser a -> Value a
parseValueText parser =
  matchValueText (first fromString . G.parseOnly parser)

text :: Value Text
text =
  matchValueText return

bytes :: Value ByteString
bytes =
  Value ask

integral :: Integral a => Value a
integral =
  parseValueBytes (F.signed F.decimal <* F.endOfInput)

double :: Value Double
double =
  parseValueBytes (F.double <* F.endOfInput)

scientific :: Value Scientific
scientific =
  parseValueBytes (F.scientific <* F.endOfInput)
