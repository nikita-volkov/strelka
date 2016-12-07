module Strelka.RequestParser where

import Strelka.Prelude
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


-- * Path segments
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


-- * Params
-------------------------

{-|
Get a parameter\'s value by its name, failing if the parameter is not present. 

@Maybe@ encodes whether the value was specified, i.e. @?name=value@ vs @?name@.
-}
getParam :: Monad m => ByteString -> RequestParser m (Maybe ByteString)
getParam name =
  do
    Request _ _ params _ _ <- A.RequestParser ask
    liftMaybe (liftM (\(ParamValue value) -> value) (G.lookup (ParamName name) params))


-- * Body
-------------------------

{-|
Consume the request body using the provided RequestBodyConsumer.

[NOTICE]
Since the body is consumed as a stream,
you can only consume it once regardless of the Alternative branching.
-}
consumeBody :: MonadIO m => P.RequestBodyConsumer a -> RequestParser m a
consumeBody (P.RequestBodyConsumer consume) =
  do
    Request _ _ _ _ (InputStream getChunk) <- A.RequestParser ask
    liftIO (consume getChunk)

{-|
Same as @'consumeBody' 'P.foldBytes'@.
-}
consumeBodyFolding :: MonadIO m => (a -> ByteString -> a) -> a -> RequestParser m a
consumeBodyFolding step init =
  consumeBody (P.foldBytes step init)

{-|
Same as @'consumeBody' 'P.build'@.
-}
consumeBodyBuilding :: (MonadIO m, Monoid a) => (ByteString -> a) -> RequestParser m a
consumeBodyBuilding proj =
  consumeBody (P.build proj)

{-|
Same as @'consumeBody' 'P.bytes'@.
-}
consumeBodyAsBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsBytes =
  consumeBody P.bytes

{-|
Same as @'consumeBody' 'P.lazyBytes'@.
-}
consumeBodyAsLazyBytes :: MonadIO m => RequestParser m B.ByteString
consumeBodyAsLazyBytes =
  consumeBody P.lazyBytes

{-|
Same as @'consumeBody' 'P.bytesBuilder'@.
-}
consumeBodyAsBytesBuilder :: MonadIO m => RequestParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBody P.bytesBuilder

{-|
Same as @'consumeBody' 'P.text'@.
-}
consumeBodyAsText :: MonadIO m => RequestParser m Text
consumeBodyAsText =
  consumeBody P.text

{-|
Same as @'consumeBody' 'P.lazyText'@.
-}
consumeBodyAsLazyText :: MonadIO m => RequestParser m L.Text
consumeBodyAsLazyText =
  consumeBody P.lazyText

{-|
Same as @'consumeBody' 'P.textBuilder'@.
-}
consumeBodyAsTextBuilder :: MonadIO m => RequestParser m M.Builder
consumeBodyAsTextBuilder =
  consumeBody P.textBuilder

{-|
Same as @'consumeBody' 'P.bytesParser'@.
-}
consumeBodyWithBytesParser :: MonadIO m => F.Parser a -> RequestParser m a
consumeBodyWithBytesParser parser =
  consumeBody (P.bytesParser parser) >>= liftEither

{-|
Same as @'consumeBody' 'P.textParser'@.
-}
consumeBodyWithTextParser :: MonadIO m => Q.Parser a -> RequestParser m a
consumeBodyWithTextParser parser =
  consumeBody (P.textParser parser) >>= liftEither
