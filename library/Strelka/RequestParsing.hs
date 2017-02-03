{-|
DSL for parsing the request.
-}
module Strelka.RequestParsing
(
  Parser,
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
  parseQuery,
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
  getAuthorization,
  -- * Body Consumption
  consumeBody,
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
import qualified Strelka.RequestBodyParsing.Parser as P
import qualified Strelka.HTTPAuthorizationParsing as D
import qualified Strelka.ParamsParsing.Params as H
import qualified URLDecoders as I


{-|
Parser of an HTTP request.
Analyzes its meta information, consumes the path segments and the body.
-}
type Parser =
  A.RequestParser


-- * Errors
-------------------------

{-|
Fail with a text message.
-}
fail :: Monad m => Text -> Parser m a
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
liftEither :: Monad m => Either Text a -> Parser m a
liftEither =
  A.RequestParser .
  lift .
  lift .
  ExceptT .
  return

{-|
Lift Maybe, interpreting Nothing as a failure.
-}
liftMaybe :: Monad m => Maybe a -> Parser m a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right

{-|
Try a parser, extracting the error as Either.
-}
unliftEither :: Monad m => Parser m a -> Parser m (Either Text a)
unliftEither =
  tryError


-- * Path Segments
-------------------------

{-|
Consume the next segment of the path.
-}
consumeSegment :: Monad m => Parser m Text
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
consumeSegmentWithParser :: Monad m => Q.Parser a -> Parser m a
consumeSegmentWithParser parser =
  consumeSegment >>= liftEither . first E.pack . Q.parseOnly parser

{-|
Consume the next segment if it matches the provided value and fail otherwise.
-}
consumeSegmentIfIs :: Monad m => Text -> Parser m ()
consumeSegmentIfIs expectedSegment =
  do
    segment <- consumeSegment
    guard (segment == expectedSegment)

{-|
Fail if there's any path segments left unconsumed.
-}
ensureThatNoSegmentsIsLeft :: Monad m => Parser m ()
ensureThatNoSegmentsIsLeft =
  A.RequestParser (lift (gets null)) >>= guard


-- * Params
-------------------------

{-|
Parse the request query,
i.e. the URL part that is between the \"?\" and \"#\" characters.
-}
parseQuery :: Monad m => H.Params a -> Parser m a
parseQuery parser =
  do
    Request _ _ (Query queryBytes) _ _ <- A.RequestParser ask
    case I.query queryBytes of
      Right query -> case H.run parser (flip G.lookup query) of
        Right result -> return result
        Left message -> fail ("Query params parsing error: " <> message)
      Left message -> fail ("Query parsing error: " <> message)


-- * Methods
-------------------------

{-|
Get the request method.
-}
getMethod :: Monad m => Parser m ByteString
getMethod =
  do
    Request (Method method) _ _ _ _ <- A.RequestParser ask
    return method

{-|
Ensure that the method matches the provided value __in lower-case__.
-}
ensureThatMethodIs :: Monad m => ByteString -> Parser m ()
ensureThatMethodIs expectedMethod =
  do
    method <- getMethod
    guard (expectedMethod == method)

{-|
Same as @'ensureThatMethodIs' "get"@.
-}
ensureThatMethodIsGet :: Monad m => Parser m ()
ensureThatMethodIsGet =
  ensureThatMethodIs "get"

{-|
Same as @'ensureThatMethodIs' "post"@.
-}
ensureThatMethodIsPost :: Monad m => Parser m ()
ensureThatMethodIsPost =
  ensureThatMethodIs "post"

{-|
Same as @'ensureThatMethodIs' "put"@.
-}
ensureThatMethodIsPut :: Monad m => Parser m ()
ensureThatMethodIsPut =
  ensureThatMethodIs "put"

{-|
Same as @'ensureThatMethodIs' "delete"@.
-}
ensureThatMethodIsDelete :: Monad m => Parser m ()
ensureThatMethodIsDelete =
  ensureThatMethodIs "delete"

{-|
Same as @'ensureThatMethodIs' "head"@.
-}
ensureThatMethodIsHead :: Monad m => Parser m ()
ensureThatMethodIsHead =
  ensureThatMethodIs "head"

{-|
Same as @'ensureThatMethodIs' "trace"@.
-}
ensureThatMethodIsTrace :: Monad m => Parser m ()
ensureThatMethodIsTrace =
  ensureThatMethodIs "trace"


-- * Headers
-------------------------

{-|
Lookup a header by name __in lower-case__.
-}
getHeader :: Monad m => ByteString -> Parser m ByteString
getHeader name =
  do
    Request _ _ _ headers _ <- A.RequestParser ask
    liftMaybe (liftM (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

{-|
Ensure that the request provides an Accept header,
which includes the specified content type.
Content type must be __in lower-case__.
-}
ensureThatAccepts :: Monad m => ByteString -> Parser m ()
ensureThatAccepts contentType =
  checkIfAccepts contentType >>=
  liftEither . bool (Left ("Unacceptable content-type: " <> fromString (show contentType))) (Right ())

{-|
Same as @'ensureThatAccepts' "text/plain"@.
-}
ensureThatAcceptsText :: Monad m => Parser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

{-|
Same as @'ensureThatAccepts' "text/html"@.
-}
ensureThatAcceptsHTML :: Monad m => Parser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

{-|
Same as @'ensureThatAccepts' "application/json"@.
-}
ensureThatAcceptsJSON :: Monad m => Parser m ()
ensureThatAcceptsJSON =
  ensureThatAccepts "application/json"

{-|
Check whether the request provides an Accept header,
which includes the specified content type.
Content type must be __in lower-case__.
-}
checkIfAccepts :: Monad m => ByteString -> Parser m Bool
checkIfAccepts contentType =
  liftM (isJust . K.matchAccept [contentType]) (getHeader "accept")

{-|
Parse the username and password from the basic authorization header.
-}
getAuthorization :: Monad m => Parser m (Text, Text)
getAuthorization =
  getHeader "authorization" >>= liftEither . D.basicCredentials


-- * Body Consumption
-------------------------

{-|
Consume the request body using the provided Parser.

[NOTICE]
Since the body is consumed as a stream,
you can only consume it once regardless of the Alternative branching.
-}
consumeBody :: MonadIO m => P.Parser a -> Parser m a
consumeBody (P.Parser consume) =
  do
    Request _ _ _ _ (InputStream getChunk) <- A.RequestParser ask
    liftIO (consume getChunk) >>= liftEither
