{-|
DSL for parsing the request.
-}
module Strelka.RequestParser
(
  RequestParser,
  -- * Errors
  fail,
  liftEither,
  liftMaybe,
  unliftEither,
  -- * Path Segments
  consumeSegment,
  consumeSegmentWithParser,
  consumeSegmentIfIs,
  ensureThatNoSegmentsIsLeft,
  -- * Params
  getParam,
  -- * Methods
  getMethod,
  ensureThatMethodIs,
  ensureThatMethodIsGet,
  ensureThatMethodIsPost,
  ensureThatMethodIsPut,
  ensureThatMethodIsDelete,
  ensureThatMethodIsHead,
  ensureThatMethodIsTrace,
  -- * Headers
  getHeader,
  ensureThatAccepts,
  ensureThatAcceptsText,
  ensureThatAcceptsHTML,
  ensureThatAcceptsJSON,
  checkIfAccepts,
  getAuthorization,
  -- * Body Consumption
  -- |
  -- [NOTICE]
  -- Since the body is consumed as a stream,
  -- you can only consume it once regardless of the Alternative branching.
  consumeBodyFoldingBytes,
  consumeBodyFoldingBytesWithTermination,
  consumeBodyBuildingFromBytes,
  consumeBodyFoldingText,
  consumeBodyFoldingTextWithTermination,
  consumeBodyBuildingFromText,
  consumeBodyAsBytes,
  consumeBodyAsLazyBytes,
  consumeBodyAsBytesBuilder,
  consumeBodyAsText,
  consumeBodyAsLazyText,
  consumeBodyAsTextBuilder,
  consumeBodyWithBytesParser,
  consumeBodyWithTextParser,
)
where

import Strelka.Prelude hiding (fail)
import Strelka.Core.Model
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Builder as C
import qualified Data.Text as E
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as M
import qualified Data.Attoparsec.ByteString as F
import qualified Data.Attoparsec.Text as Q
import qualified Data.HashMap.Strict as G
import qualified Network.HTTP.Media as K
import qualified Strelka.Core.RequestParser as A
import qualified Strelka.RequestBodyConsumer as P
import qualified Strelka.HTTPAuthorizationParser as D


{-|
Parser of an HTTP request.
Analyzes its meta information, consumes the path segments and the body.
-}
type RequestParser =
  A.RequestParser


-- * Errors
-------------------------

{-|
Fail with a text message.
-}
fail :: Monad m => Text -> RequestParser m a
fail message =
  A.RequestParser $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

{-|
Lift Either, interpreting Left as a failure.
-}
liftEither :: Monad m => Either Text a -> RequestParser m a
liftEither =
  A.RequestParser .
  lift .
  lift .
  ExceptT .
  return

{-|
Lift Maybe, interpreting Nothing as a failure.
-}
liftMaybe :: Monad m => Maybe a -> RequestParser m a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right

{-|
Try a parser, extracting the error as Either.
-}
unliftEither :: Monad m => RequestParser m a -> RequestParser m (Either Text a)
unliftEither =
  tryError


-- * Path Segments
-------------------------

{-|
Consume the next segment of the path.
-}
consumeSegment :: Monad m => RequestParser m Text
consumeSegment =
  A.RequestParser $
  lift $
  StateT $
  \case
    PathSegment segmentText : segmentsTail ->
      return (segmentText, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

{-|
Consume the next segment of the path with Attoparsec parser.
-}
consumeSegmentWithParser :: Monad m => Q.Parser a -> RequestParser m a
consumeSegmentWithParser parser =
  consumeSegment >>= liftEither . first E.pack . Q.parseOnly parser

{-|
Consume the next segment if it matches the provided value and fail otherwise.
-}
consumeSegmentIfIs :: Monad m => Text -> RequestParser m ()
consumeSegmentIfIs expectedSegment =
  do
    segment <- consumeSegment
    guard (segment == expectedSegment)

{-|
Fail if there's any path segments left unconsumed.
-}
ensureThatNoSegmentsIsLeft :: Monad m => RequestParser m ()
ensureThatNoSegmentsIsLeft =
  A.RequestParser (lift (gets null)) >>= guard


-- * Params
-------------------------

{-|
Get a parameter\'s value by its name, failing if the parameter is not present. 

@Maybe@ encodes whether a value was specified at all, i.e. @?name=value@ vs @?name@.
-}
getParam :: Monad m => ByteString -> RequestParser m (Maybe ByteString)
getParam name =
  do
    Request _ _ params _ _ <- A.RequestParser ask
    liftMaybe (liftM (\(ParamValue value) -> value) (G.lookup (ParamName name) params))


-- * Methods
-------------------------

{-|
Get the request method.
-}
getMethod :: Monad m => RequestParser m ByteString
getMethod =
  do
    Request (Method method) _ _ _ _ <- A.RequestParser ask
    return method

{-|
Ensure that the method matches the provided value in lower-case.
-}
ensureThatMethodIs :: Monad m => ByteString -> RequestParser m ()
ensureThatMethodIs expectedMethod =
  do
    method <- getMethod
    guard (expectedMethod == method)

{-|
Same as @'ensureThatMethodIs' "get"@.
-}
ensureThatMethodIsGet :: Monad m => RequestParser m ()
ensureThatMethodIsGet =
  ensureThatMethodIs "get"

{-|
Same as @'ensureThatMethodIs' "post"@.
-}
ensureThatMethodIsPost :: Monad m => RequestParser m ()
ensureThatMethodIsPost =
  ensureThatMethodIs "post"

{-|
Same as @'ensureThatMethodIs' "put"@.
-}
ensureThatMethodIsPut :: Monad m => RequestParser m ()
ensureThatMethodIsPut =
  ensureThatMethodIs "put"

{-|
Same as @'ensureThatMethodIs' "delete"@.
-}
ensureThatMethodIsDelete :: Monad m => RequestParser m ()
ensureThatMethodIsDelete =
  ensureThatMethodIs "delete"

{-|
Same as @'ensureThatMethodIs' "head"@.
-}
ensureThatMethodIsHead :: Monad m => RequestParser m ()
ensureThatMethodIsHead =
  ensureThatMethodIs "head"

{-|
Same as @'ensureThatMethodIs' "trace"@.
-}
ensureThatMethodIsTrace :: Monad m => RequestParser m ()
ensureThatMethodIsTrace =
  ensureThatMethodIs "trace"


-- * Headers
-------------------------

{-|
Lookup a header by name in lower-case.
-}
getHeader :: Monad m => ByteString -> RequestParser m ByteString
getHeader name =
  do
    Request _ _ _ headers _ <- A.RequestParser ask
    liftMaybe (liftM (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

{-|
Ensure that the request provides an Accept header,
which includes the specified content type.
Content type must be in lower-case.
-}
ensureThatAccepts :: Monad m => ByteString -> RequestParser m ()
ensureThatAccepts contentType =
  checkIfAccepts contentType >>=
  liftEither . bool (Left ("Unacceptable content-type: " <> fromString (show contentType))) (Right ())

{-|
Same as @'ensureThatAccepts' "text/plain"@.
-}
ensureThatAcceptsText :: Monad m => RequestParser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

{-|
Same as @'ensureThatAccepts' "text/html"@.
-}
ensureThatAcceptsHTML :: Monad m => RequestParser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

{-|
Same as @'ensureThatAccepts' "application/json"@.
-}
ensureThatAcceptsJSON :: Monad m => RequestParser m ()
ensureThatAcceptsJSON =
  ensureThatAccepts "application/json"

{-|
Check whether the request provides an Accept header,
which includes the specified content type.
Content type must be in lower-case.
-}
checkIfAccepts :: Monad m => ByteString -> RequestParser m Bool
checkIfAccepts contentType =
  liftM (isJust . K.matchAccept [contentType]) (getHeader "accept")

{-|
Parse the username and password from the basic authorization header.
-}
getAuthorization :: Monad m => RequestParser m (Text, Text)
getAuthorization =
  getHeader "authorization" >>= liftEither . D.basicCredentials


-- * Body Consumption
-------------------------

{-|
Consume the request body using the provided RequestBodyConsumer.
-}
consumeBody :: MonadIO m => P.RequestBodyConsumer a -> RequestParser m a
consumeBody (P.RequestBodyConsumer consume) =
  do
    Request _ _ _ _ (InputStream getChunk) <- A.RequestParser ask
    liftIO (consume getChunk)

{-|
Consume the request body by folding over the chunks of the byte-stream.
-}
consumeBodyFoldingBytes :: MonadIO m => (a -> ByteString -> a) -> a -> RequestParser m a
consumeBodyFoldingBytes step init =
  consumeBody (P.foldBytes step init)

{-|
Consume the request body by folding over the chunks of the byte-stream,
with support for early termination.
The termination is interpreted from "Left".
-}
consumeBodyFoldingBytesWithTermination :: MonadIO m => (a -> ByteString -> Either a a) -> a -> RequestParser m a
consumeBodyFoldingBytesWithTermination step init =
  consumeBody (P.foldBytesWithTermination step init)

{-|
Consume the request body by building a Monoid value from the chunks of the byte-stream.
-}
consumeBodyBuildingFromBytes :: (MonadIO m, Monoid a) => (ByteString -> a) -> RequestParser m a
consumeBodyBuildingFromBytes proj =
  consumeBody (P.buildFromBytes proj)

{-|
Consume the request body by folding over the chunks of the input stream decoded using UTF8.
-}
consumeBodyFoldingText :: MonadIO m => (a -> Text -> a) -> a -> RequestParser m a
consumeBodyFoldingText step init =
  consumeBody (P.foldText step init)

{-|
Consume the request body by folding over the chunks of the input stream decoded using UTF8,
with support for early termination.
The termination is interpreted from "Left".
-}
consumeBodyFoldingTextWithTermination :: MonadIO m => (a -> Text -> Either a a) -> a -> RequestParser m a
consumeBodyFoldingTextWithTermination step init =
  consumeBody (P.foldTextWithTermination step init)

{-|
Consume the request body by building a Monoid value from the chunks of the input stream decoded using UTF8.
-}
consumeBodyBuildingFromText :: (MonadIO m, Monoid a) => (Text -> a) -> RequestParser m a
consumeBodyBuildingFromText proj =
  consumeBody (P.buildFromText proj)

{-|
Consume the whole body as bytes.
-}
consumeBodyAsBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsBytes =
  consumeBody P.bytes

{-|
Consume the whole body as lazy bytes.
-}
consumeBodyAsLazyBytes :: MonadIO m => RequestParser m B.ByteString
consumeBodyAsLazyBytes =
  consumeBody P.lazyBytes

{-|
Consume the whole body as a bytes builder.
-}
consumeBodyAsBytesBuilder :: MonadIO m => RequestParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBody P.bytesBuilder

{-|
Consume the whole body as a UTF8-decoded text.
-}
consumeBodyAsText :: MonadIO m => RequestParser m Text
consumeBodyAsText =
  consumeBody P.text

{-|
Consume the whole body as a UTF8-decoded lazy text.
-}
consumeBodyAsLazyText :: MonadIO m => RequestParser m L.Text
consumeBodyAsLazyText =
  consumeBody P.lazyText

{-|
Consume the whole body as a UTF8-decoded text builder.
-}
consumeBodyAsTextBuilder :: MonadIO m => RequestParser m M.Builder
consumeBodyAsTextBuilder =
  consumeBody P.textBuilder

{-|
Consume the body with an Attoparsec bytes parser,
terminating when it requires no more input.
-}
consumeBodyWithBytesParser :: MonadIO m => F.Parser a -> RequestParser m a
consumeBodyWithBytesParser parser =
  consumeBody (P.bytesParser parser) >>= liftEither

{-|
Consume the body with an Attoparsec text parser,
terminating when it requires no more input.
-}
consumeBodyWithTextParser :: MonadIO m => Q.Parser a -> RequestParser m a
consumeBodyWithTextParser parser =
  consumeBody (P.textParser parser) >>= liftEither
