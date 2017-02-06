{-|
DSL for parsing the request.
-}
module Strelka.RequestParsing
(
  Parser,
  -- * Errors
  fail,
  try,
  -- * Path Segments
  segment,
  segmentWithParser,
  segmentIs,
  noSegmentsLeft,
  -- * Query
  query1,
  query2,
  query3,
  query4,
  query5,
  query6,
  query7,
  queryWithParser,
  -- * Methods
  method,
  methodIs,
  methodIsGet,
  methodIsPost,
  methodIsPut,
  methodIsDelete,
  methodIsHead,
  methodIsTrace,
  -- * Headers
  header,
  accepts,
  acceptsText,
  acceptsHTML,
  acceptsJSON,
  authorization,
  -- * Body Consumption
  body,
  bodyWithParser,
)
where

import Strelka.Prelude hiding (fail, try)
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
import qualified Strelka.RequestBodyParsing as N
import qualified Strelka.HTTPAuthorizationParsing as D
import qualified Strelka.ParamsParsing.Params as H
import qualified Strelka.ParamsParsing as O
import qualified URLDecoders as I
import qualified Attoparsec.Data.Implicit as J


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
try :: Monad m => Parser m a -> Parser m (Either Text a)
try =
  tryError


-- * Path Segments
-------------------------

{-|
Consume the next segment of the path as Text.
If you need Text it's more efficient than using 'segment'.
-}
segmentText :: Monad m => Parser m Text
segmentText =
  A.RequestParser $
  lift $
  StateT $
  \case
    PathSegment segmentText : segmentsTail ->
      return (segmentText, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

{-|
Consume the next segment if it matches the provided value and fail otherwise.
-}
segmentIs :: Monad m => Text -> Parser m ()
segmentIs expectedSegment =
  do
    segment <- segmentText
    guard (segment == expectedSegment)

{-|
Consume the next segment of the path with an explicit Attoparsec parser.
-}
segmentWithParser :: Monad m => Q.Parser a -> Parser m a
segmentWithParser parser =
  A.RequestParser $
  lift $
  StateT $
  \case
    PathSegment segmentText : segmentsTail ->
      case Q.parseOnly parser segmentText of
        Right result -> return (result, segmentsTail)
        Left failure -> ExceptT (return (Left ("Segment \"" <> segmentText <> "\" parsing failure: " <> E.pack failure)))
    _ ->
      ExceptT (return (Left "No segments left"))

{-|
Consume the next segment of the path with an implicit lenient Attoparsec parser.
-}
segment :: (Monad m, J.LenientParser a) => Parser m a
segment =
  segmentWithParser (J.lenientParser <* Q.endOfInput)

{-|
Fail if there's any path segments left unconsumed.
-}
noSegmentsLeft :: Monad m => Parser m ()
noSegmentsLeft =
  A.RequestParser (lift (gets null)) >>= guard


-- * Query
-------------------------

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query1 #-}
query1 :: (Monad m, O.DefaultValue a) => Text -> Parser m a
query1 name1 =
  queryWithParser (O.defaultParams1 name1)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query2 #-}
query2 :: (Monad m, O.DefaultValue a, O.DefaultValue b) => Text -> Text -> Parser m (a, b)
query2 name1 name2 =
  queryWithParser (O.defaultParams2 name1 name2)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query3 #-}
query3 :: (Monad m, O.DefaultValue a, O.DefaultValue b, O.DefaultValue c) => Text -> Text -> Text -> Parser m (a, b, c)
query3 name1 name2 name3 =
  queryWithParser (O.defaultParams3 name1 name2 name3)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query4 #-}
query4 :: (Monad m, O.DefaultValue a, O.DefaultValue b, O.DefaultValue c, O.DefaultValue d) => Text -> Text -> Text -> Text -> Parser m (a, b, c, d)
query4 name1 name2 name3 name4 =
  queryWithParser (O.defaultParams4 name1 name2 name3 name4)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query5 #-}
query5 :: (Monad m, O.DefaultValue a, O.DefaultValue b, O.DefaultValue c, O.DefaultValue d, O.DefaultValue e) => Text -> Text -> Text -> Text -> Text -> Parser m (a, b, c, d, e)
query5 name1 name2 name3 name4 name5 =
  queryWithParser (O.defaultParams5 name1 name2 name3 name4 name5)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query6 #-}
query6 :: (Monad m, O.DefaultValue a, O.DefaultValue b, O.DefaultValue c, O.DefaultValue d, O.DefaultValue e, O.DefaultValue f) => Text -> Text -> Text -> Text -> Text -> Text -> Parser m (a, b, c, d, e, f)
query6 name1 name2 name3 name4 name5 name6 =
  queryWithParser (O.defaultParams6 name1 name2 name3 name4 name5 name6)

{-|
Parse the query using implicit parsers by specifying the names of parameters.
-}
{-# INLINE query7 #-}
query7 :: (Monad m, O.DefaultValue a, O.DefaultValue b, O.DefaultValue c, O.DefaultValue d, O.DefaultValue e, O.DefaultValue f, O.DefaultValue g) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Parser m (a, b, c, d, e, f, g)
query7 name1 name2 name3 name4 name5 name6 name7 =
  queryWithParser (O.defaultParams7 name1 name2 name3 name4 name5 name6 name7)

{-|
Parse the request query,
i.e. the URL part that is between the \"?\" and \"#\" characters,
with an explicitly specified parser.
-}
{-# INLINE queryWithParser #-}
queryWithParser :: Monad m => H.Params a -> Parser m a
queryWithParser parser =
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
method :: Monad m => Parser m ByteString
method =
  do
    Request (Method method) _ _ _ _ <- A.RequestParser ask
    return method

{-|
Ensure that the method matches the provided value __in lower-case__.
-}
methodIs :: Monad m => ByteString -> Parser m ()
methodIs expectedMethod =
  do
    method <- method
    guard (expectedMethod == method)

{-|
Same as @'methodIs' "get"@.
-}
methodIsGet :: Monad m => Parser m ()
methodIsGet =
  methodIs "get"

{-|
Same as @'methodIs' "post"@.
-}
methodIsPost :: Monad m => Parser m ()
methodIsPost =
  methodIs "post"

{-|
Same as @'methodIs' "put"@.
-}
methodIsPut :: Monad m => Parser m ()
methodIsPut =
  methodIs "put"

{-|
Same as @'methodIs' "delete"@.
-}
methodIsDelete :: Monad m => Parser m ()
methodIsDelete =
  methodIs "delete"

{-|
Same as @'methodIs' "head"@.
-}
methodIsHead :: Monad m => Parser m ()
methodIsHead =
  methodIs "head"

{-|
Same as @'methodIs' "trace"@.
-}
methodIsTrace :: Monad m => Parser m ()
methodIsTrace =
  methodIs "trace"


-- * Headers
-------------------------

{-|
Lookup a header by name __in lower-case__.
-}
header :: Monad m => ByteString -> Parser m ByteString
header name =
  do
    Request _ _ _ headers _ <- A.RequestParser ask
    liftMaybe (liftM (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

{-|
Ensure that the request provides an Accept header,
which includes the specified content type.
Content type must be __in lower-case__.
-}
accepts :: Monad m => ByteString -> Parser m ()
accepts contentType =
  checkIfAccepts contentType >>=
  liftEither . bool (Left ("Unacceptable content-type: " <> fromString (show contentType))) (Right ())

{-|
Same as @'accepts' "text/plain"@.
-}
acceptsText :: Monad m => Parser m ()
acceptsText =
  accepts "text/plain"

{-|
Same as @'accepts' "text/html"@.
-}
acceptsHTML :: Monad m => Parser m ()
acceptsHTML =
  accepts "text/html"

{-|
Same as @'accepts' "application/json"@.
-}
acceptsJSON :: Monad m => Parser m ()
acceptsJSON =
  accepts "application/json"

{-|
Check whether the request provides an Accept header,
which includes the specified content type.
Content type must be __in lower-case__.
-}
checkIfAccepts :: Monad m => ByteString -> Parser m Bool
checkIfAccepts contentType =
  liftM (isJust . K.matchAccept [contentType]) (header "accept")

{-|
Parse the username and password from the basic authorization header.
-}
authorization :: Monad m => Parser m (Text, Text)
authorization =
  header "authorization" >>= liftEither . D.basicCredentials


-- * Body Consumption
-------------------------

{-|
Consume the request body using an implicit parser.

[NOTICE]
Since the body is consumed as a stream,
you can only consume it once regardless of the Alternative branching.
-}
body :: (MonadIO m, N.DefaultParser a) => Parser m a
body =
  bodyWithParser N.defaultParser

{-|
Consume the request body using the explicitly specified parser.

[NOTICE]
Since the body is consumed as a stream,
you can only consume it once regardless of the Alternative branching.
-}
bodyWithParser :: MonadIO m => P.Parser a -> Parser m a
bodyWithParser (P.Parser consume) =
  do
    Request _ _ _ _ (InputStream getChunk) <- A.RequestParser ask
    liftIO (consume getChunk) >>= liftEither
