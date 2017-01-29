{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParser where

import Strelka.Prelude
import qualified Strelka.HTTPAuthorizationParser as D
import qualified Data.Attoparsec.ByteString.Char8 as F
import qualified Data.Attoparsec.Zepto as E
import qualified Data.Attoparsec.Text as G
import qualified Data.Text.Encoding as I
import qualified Data.Text.Encoding.Error as J
import qualified Data.HashMap.Strict as K


newtype Params a =
  Params (ReaderT (HashMap Text ByteString) (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

param :: Text -> Value a -> Params a
param name valueParser =
  Params (ReaderT readerFn)
  where
    readerFn map =
      lookup >>= parse
      where
        lookup =
          (except . maybe (Left error) Right . K.lookup name) map
          where
            error =
              "Param not found: " <> name
        parse =
          withExcept updateError . valueReaderFn
          where
            valueReaderFn =
              case valueParser of Value (ReaderT x) -> x
            updateError error =
              "Param \"" <> name <> "\" parsing error: " <> error

newtype Value a =
  Value (ReaderT ByteString (Except Text) a)
  deriving (Functor, Applicative, Alternative)

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

{-|
Accepts any string interpretable as a boolean:
"1" or "0", "true" or "false", "yes" or "no", "y" or "n", "t" or "f".
Case-insensitive.
-}
bool :: Value Bool
bool =
  parseValueBytes parser
  where
    parser =
      (one <|> zero <|> true <|> false <|> yes <|> no <|> y <|> n <|> t <|> f) <* F.endOfInput
      where
        one =
          F.char '1' $> True
        zero =
          F.char '0' $> False
        true =
          F.stringCI "true" $> True
        false =
          F.stringCI "false" $> False
        yes =
          F.stringCI "yes" $> True
        no =
          F.stringCI "no" $> False
        t =
          F.satisfy (F.inClass "tT") $> True
        f =
          F.satisfy (F.inClass "fF") $> False
        y =
          F.satisfy (F.inClass "yY") $> True
        n =
          F.satisfy (F.inClass "nN") $> False

char :: Value Char
char =
  parseValueText (G.anyChar <* G.endOfInput)
