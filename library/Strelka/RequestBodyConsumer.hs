module Strelka.RequestBodyConsumer
where

import Strelka.Prelude
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Strelka.ParamsParser


{-|
A specification of how to consume the request body byte-stream.
-}
newtype RequestBodyConsumer a =
  RequestBodyConsumer (IO ByteString -> IO a)
  deriving (Functor)

{-|
Fold with support for early termination,
which is interpreted from "Left".
-}
foldBytesWithTermination :: (a -> ByteString -> Either a a) -> a -> RequestBodyConsumer a
foldBytesWithTermination step init =
  RequestBodyConsumer consumer
  where
    consumer getChunk =
      recur init
      where
        recur state =
          getChunk >>= onChunk
          where
            onChunk chunk =
              if Data.ByteString.null chunk
                then return state
                else case step state chunk of
                  Left newState -> return newState
                  Right newState -> recur newState

{-|
Fold with support for early termination,
which is interpreted from "Left".
-}
foldTextWithTermination :: (a -> Text -> Either a a) -> a -> RequestBodyConsumer a
foldTextWithTermination step init =
  fmap snd (foldBytesWithTermination bytesStep bytesInit)
  where
    bytesInit =
      (decode, init)
      where
        decode =
          Data.Text.Encoding.streamDecodeUtf8With Data.Text.Encoding.Error.lenientDecode
    bytesStep (!decode, !state) bytesChunk =
      case decode bytesChunk of
        Data.Text.Encoding.Some textChunk leftovers nextDecode ->
          if Data.Text.null textChunk
            then Right (nextDecode, state)
            else bimap ((,) nextDecode) ((,) nextDecode) (step state textChunk)

{-|
Fold over ByteString chunks.
-}
foldBytes :: (a -> ByteString -> a) -> a -> RequestBodyConsumer a
foldBytes step init =
  RequestBodyConsumer consumer
  where
    consumer getChunk =
      recur init
      where
        recur state =
          getChunk >>= onChunk
          where
            onChunk chunk =
              if Data.ByteString.null chunk
                then return state
                else recur (step state chunk)

{-|
Fold over text chunks decoded using UTF8.
-}
foldText :: (a -> Text -> a) -> a -> RequestBodyConsumer a
foldText step init =
  fmap fst (foldBytes bytesStep bytesInit)
  where
    bytesInit =
      (init, Data.Text.Encoding.streamDecodeUtf8With Data.Text.Encoding.Error.lenientDecode)
    bytesStep (!state, !decode) bytesChunk =
      case decode bytesChunk of
        Data.Text.Encoding.Some textChunk leftovers nextDecode ->
          (nextState, nextDecode)
          where
            nextState =
              if Data.Text.null textChunk
                then state
                else step state textChunk

{- |
Similar to "Foldable"\'s 'foldMap'.
-}
buildFromBytes :: Monoid a => (ByteString -> a) -> RequestBodyConsumer a
buildFromBytes proj =
  foldBytes (\l r -> mappend l (proj r)) mempty

{- |
Similar to "Foldable"\'s 'foldMap'.
-}
buildFromText :: Monoid a => (Text -> a) -> RequestBodyConsumer a
buildFromText proj =
  foldText (\l r -> mappend l (proj r)) mempty

{-|
Consume as ByteString.
-}
bytes :: RequestBodyConsumer ByteString
bytes =
  fmap Data.ByteString.Lazy.toStrict lazyBytes

{-|
Consume as lazy ByteString.
-}
lazyBytes :: RequestBodyConsumer Data.ByteString.Lazy.ByteString
lazyBytes =
  fmap Data.ByteString.Builder.toLazyByteString bytesBuilder

{-|
Consume as ByteString Builder.
-}
bytesBuilder :: RequestBodyConsumer Data.ByteString.Builder.Builder
bytesBuilder =
  buildFromBytes Data.ByteString.Builder.byteString

{-|
Consume as Text.
-}
text :: RequestBodyConsumer Text
text =
  fmap Data.Text.Lazy.toStrict lazyText

{-|
Consume as lazy Text.
-}
lazyText :: RequestBodyConsumer Data.Text.Lazy.Text
lazyText =
  fmap Data.Text.Lazy.Builder.toLazyText textBuilder

{-|
Consume as Text Builder.
-}
textBuilder :: RequestBodyConsumer Data.Text.Lazy.Builder.Builder
textBuilder =
  fmap fst (foldBytes step init)
  where
    step (builder, decode) bytes =
      case decode bytes of
        Data.Text.Encoding.Some decodedChunk _ newDecode ->
          (builder <> Data.Text.Lazy.Builder.fromText decodedChunk, newDecode)
    init =
      (mempty, Data.Text.Encoding.streamDecodeUtf8)

{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
bytesParser :: Data.Attoparsec.ByteString.Parser a -> RequestBodyConsumer (Either Text a)
bytesParser parser =
  parserResult foldBytesWithTermination (Data.Attoparsec.ByteString.Partial (Data.Attoparsec.ByteString.parse parser))

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
textParser :: Data.Attoparsec.Text.Parser a -> RequestBodyConsumer (Either Text a)
textParser parser =
  parserResult foldTextWithTermination (Data.Attoparsec.Text.Partial (Data.Attoparsec.Text.parse parser))

{-|
Given a chunk-specialized terminating fold implementation lifts a generic Attoparsec result.
-}
parserResult :: Monoid i => (forall a. (a -> i -> Either a a) -> a -> RequestBodyConsumer a) -> Data.Attoparsec.Types.IResult i a -> RequestBodyConsumer (Either Text a)
parserResult fold result =
  fmap finalise (fold step result)
  where
    step result chunk =
      case result of
        Data.Attoparsec.Types.Partial chunkToResult ->
          Right (chunkToResult chunk)
        _ ->
          Left result
    finalise =
      \case
        Data.Attoparsec.Types.Partial chunkToResult ->
          finalise (chunkToResult mempty)
        Data.Attoparsec.Types.Done leftovers resultValue ->
          Right resultValue
        Data.Attoparsec.Types.Fail leftovers contexts message ->
          Left (fromString (intercalate " > " contexts <> ": " <> message))

-- |
-- Consumes the input stream as an \"application/x-www-form-urlencoded\"
-- association list of parameters.
paramsParser :: Strelka.ParamsParser.Params a -> RequestBodyConsumer (Either Text a)
paramsParser (Strelka.ParamsParser.Params p) =
  undefined
