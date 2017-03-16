module Strelka.RequestBodyParsing.Parser
where

import Strelka.Prelude hiding (fail)
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Builder
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Internal.Lazy
import qualified Data.HashMap.Strict as C
import qualified Strelka.ParamsParsing.Params as A
import qualified URLDecoders as B
import qualified Text.Builder as D
import qualified ByteString.TreeBuilder as E


{-|
A specification of how to consume the request body byte-stream.
-}
newtype Parser a =
  Parser (IO ByteString -> IO (Either Text a))
  deriving (Functor)

instance Applicative Parser where
  pure =
    return
  (<*>) =
    ap

instance Monad Parser where
  return x =
    Parser (\_ -> pure (Right x))
  (>>=) (Parser def1) cont2 =
    Parser def
    where
      def input =
        def1 input >>= \case
          Right result -> case cont2 result of Parser def2 -> def2 input
          Left failure -> return (Left failure)

{-|
Result of a folding step.
-}
data Folded a =
  Unfinished !a |
  Finished !a |
  Failed Text
  deriving (Functor)

{-|
Fail with a message.
-}
{-# INLINE fail #-}
fail :: Text -> Parser a
fail message =
  Parser (\_ -> return (Left message))

{-|
Fold with support for early termination and failure.
-}
{-# INLINABLE foldBytes #-}
foldBytes :: (a -> ByteString -> Folded a) -> a -> Parser a
foldBytes step init =
  Parser consumer
  where
    consumer getChunk =
      recur init
      where
        recur !state =
          getChunk >>= onChunk
          where
            onChunk chunk =
              if Data.ByteString.null chunk
                then return (Right state)
                else case step state chunk of
                  Unfinished newState -> recur newState
                  Finished newState -> return (Right newState)
                  Failed failure -> return (Left failure)

{-|
Fold with support for early termination and failure.
-}
{-# INLINABLE foldText #-}
foldText :: (a -> Text -> Folded a) -> a -> Parser a
foldText step init =
  Parser consumer
  where
    consumer getChunk =
      recur Data.Text.Encoding.streamDecodeUtf8 init
      where
        recur !decode !accumulator =
          do
            chunk <- getChunk
            if Data.ByteString.null chunk
              then return (Right accumulator)
              else catch (decodeChunk chunk) fail
          where
            decodeChunk chunk =
              case decode chunk of
                Data.Text.Encoding.Some textChunk leftovers newDecode ->
                  if Data.Text.null textChunk
                    then recur newDecode accumulator
                    else case step accumulator textChunk of
                      Unfinished newAccumulator -> recur newDecode newAccumulator
                      Finished newAccumulator -> return (Right accumulator)
                      Failed failure -> return (Left failure)
            fail (Data.Text.Encoding.Error.DecodeError message byte) =
              return (Left ("UTF8 decoding failure: " <> fromString message))

{- |
Fold over the input chunks, projecting them into a monoid.
Similar to "Foldable"\'s 'foldMap'.
-}
{-# INLINE buildFromBytes #-}
buildFromBytes :: Monoid a => (ByteString -> a) -> Parser a
buildFromBytes proj =
  foldBytes (\l r -> Unfinished (mappend l (proj r))) mempty

{- |
Fold over the input chunks, projecting them into a monoid.
Similar to "Foldable"\'s 'foldMap'.
-}
{-# INLINE buildFromText #-}
buildFromText :: Monoid a => (Text -> a) -> Parser a
buildFromText proj =
  foldText (\l r -> Unfinished (mappend l (proj r))) mempty

{-|
Consume as ByteString.
-}
{-# INLINE bytes #-}
bytes :: Parser ByteString
bytes =
  fmap E.toByteString bytesBuilder

{-|
Consume as a strict ByteString builder.
-}
{-# INLINE bytesBuilder #-}
bytesBuilder :: Parser E.Builder
bytesBuilder =
  buildFromBytes E.byteString

{-|
Consume as lazy ByteString.
-}
{-# INLINE lazyBytes #-}
lazyBytes :: Parser Data.ByteString.Lazy.ByteString
lazyBytes =
  fmap fromAccumulator (buildFromBytes toAccumulator)
  where
    toAccumulator chunk =
      Endo (Data.ByteString.Lazy.Internal.Chunk chunk)
    fromAccumulator (Endo fn) =
      fn Data.ByteString.Lazy.Internal.Empty

{-|
Consume as a lazy ByteString builder.
-}
{-# INLINE lazyBytesBuilder #-}
lazyBytesBuilder :: Parser Data.ByteString.Builder.Builder
lazyBytesBuilder =
  buildFromBytes Data.ByteString.Builder.byteString

{-|
Consume as Text.
-}
{-# INLINE text #-}
text :: Parser Text
text =
  fmap D.run textBuilder

{-|
Consume as a strict Text builder.
-}
{-# INLINE textBuilder #-}
textBuilder :: Parser D.Builder
textBuilder =
  buildFromText D.text

{-|
Consume as lazy Text.
-}
{-# INLINE lazyText #-}
lazyText :: Parser Data.Text.Lazy.Text
lazyText =
  fmap fromAccumulator (buildFromText toAccumulator)
  where
    toAccumulator chunk =
      Endo (Data.Text.Internal.Lazy.Chunk chunk)
    fromAccumulator (Endo fn) =
      fn Data.Text.Internal.Lazy.Empty

{-|
Consume as a lazy Text builder.
-}
{-# INLINE lazyTextBuilder #-}
lazyTextBuilder :: Parser Data.Text.Lazy.Builder.Builder
lazyTextBuilder =
  buildFromText Data.Text.Lazy.Builder.fromText

{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: Data.Attoparsec.ByteString.Parser a -> Parser a
parseBytes parser =
  processParserResult foldBytes (Data.Attoparsec.ByteString.Partial (Data.Attoparsec.ByteString.parse parser))

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: Data.Attoparsec.Text.Parser a -> Parser a
parseText parser =
  processParserResult foldText (Data.Attoparsec.Text.Partial (Data.Attoparsec.Text.parse parser))

{-|
Given a chunk-specialized terminating fold implementation lifts a generic Attoparsec result.
-}
{-# INLINE processParserResult #-}
processParserResult :: Monoid chunk => (forall a. (a -> chunk -> Folded a) -> a -> Parser a) -> Data.Attoparsec.Types.IResult chunk a -> Parser a
processParserResult fold result =
  fold step result >>= finalise
  where
    step result chunk =
      case result of
        Data.Attoparsec.Types.Partial chunkToResult ->
          Unfinished (chunkToResult chunk)
        _ ->
          Finished result
    finalise =
      \case
        Data.Attoparsec.Types.Done leftovers resultValue ->
          Parser (\_ -> return (Right resultValue))
        Data.Attoparsec.Types.Fail leftovers contexts message ->
          Parser (\_ -> return (Left (fromString (intercalate " > " contexts <> ": " <> message))))
        Data.Attoparsec.Types.Partial chunkToResult ->
          finalise (chunkToResult mempty)

{-|
Parses the input stream as \"application/x-www-form-urlencoded\".
-}
{-# INLINE parseParams #-}
parseParams :: A.Params a -> Parser a
parseParams parser =
  do
    queryBytes <- bytes
    case B.utf8Query queryBytes of
      Right query -> case A.run parser (flip C.lookup query) of
        Right result -> return result
        Left message -> fail ("Query params parsing error: " <> message)
      Left message -> fail ("Query parsing error: " <> message)
