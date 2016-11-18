module Router.InputStreamConsumer where

import Router.Prelude
import Router.Model
import qualified Data.Attoparsec.ByteString
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Router.ParamsParser


newtype InputStreamConsumer a =
  InputStreamConsumer (IO ByteString -> IO a)
  deriving (Functor)

folding :: (a -> ByteString -> a) -> a -> InputStreamConsumer a
folding step init =
  InputStreamConsumer consumer
  where
    consumer getChunk =
      recur init
      where
        recur acc =
          getChunk >>= onChunk
          where
            onChunk chunk =
              if Data.ByteString.null chunk
                then return acc
                else recur (step acc chunk)

building :: Monoid builder => (ByteString -> builder) -> InputStreamConsumer builder
building proj =
  folding (\l r -> mappend l (proj r)) mempty

bytes :: InputStreamConsumer ByteString
bytes =
  fmap Data.ByteString.Lazy.toStrict lazyBytes

lazyBytes :: InputStreamConsumer Data.ByteString.Lazy.ByteString
lazyBytes =
  fmap Data.ByteString.Builder.toLazyByteString bytesBuilder

bytesBuilder :: InputStreamConsumer Data.ByteString.Builder.Builder
bytesBuilder =
  building Data.ByteString.Builder.byteString

text :: InputStreamConsumer Text
text =
  fmap Data.Text.Lazy.toStrict lazyText

lazyText :: InputStreamConsumer Data.Text.Lazy.Text
lazyText =
  fmap Data.Text.Lazy.Builder.toLazyText textBuilder

textBuilder :: InputStreamConsumer Data.Text.Lazy.Builder.Builder
textBuilder =
  fmap fst (folding step init)
  where
    step (builder, decode) bytes =
      case decode bytes of
        Data.Text.Encoding.Some decodedChunk _ newDecode ->
          (builder <> Data.Text.Lazy.Builder.fromText decodedChunk, newDecode)
    init =
      (mempty, Data.Text.Encoding.streamDecodeUtf8)

-- |
-- Turn a bytes parser into an input stream consumer.
attoparsecBytesParser :: Data.Attoparsec.ByteString.Parser a -> InputStreamConsumer (Either Text a)
attoparsecBytesParser parser =
  attoparsecResult (Data.Attoparsec.ByteString.Partial (Data.Attoparsec.ByteString.parse parser))

attoparsecResult :: Data.Attoparsec.ByteString.Result a -> InputStreamConsumer (Either Text a)
attoparsecResult result =
  fmap finalise (folding step result)
  where
    step result chunk =
      case result of
        Data.Attoparsec.ByteString.Partial chunkToResult ->
          chunkToResult chunk
        _ ->
          result
    finalise =
      \case
        Data.Attoparsec.ByteString.Partial chunkToResult ->
          finalise (chunkToResult mempty)
        Data.Attoparsec.ByteString.Done leftovers resultValue ->
          Right resultValue
        Data.Attoparsec.ByteString.Fail leftovers contexts message ->
          Left (fromString (intercalate " > " contexts <> ": " <> message))

-- |
-- Consumes the input stream as an \"application/x-www-form-urlencoded\"
-- association list of parameters.
paramsParser :: Router.ParamsParser.ParamsParser a -> InputStreamConsumer (Either Text a)
paramsParser (Router.ParamsParser.ParamsParser p) =
  attoparsecBytesParser p
