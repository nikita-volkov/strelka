module Strelka.RequestParser where

import Strelka.Prelude
import Strelka.Model
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Builder as C
import qualified Data.Text as E
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as M
import qualified Data.Attoparsec.ByteString as F
import qualified Data.Attoparsec.Text as Q
import qualified Data.HashMap.Strict as G
import qualified Network.HTTP.Media as K
import qualified Strelka.RequestBodyConsumer as P
import qualified Strelka.HTTPAuthorizationParser as D


newtype RequestParser m a =
  RequestParser (ReaderT Request (StateT [Text] (ExceptT Text m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError Text)

instance MonadIO m => MonadIO (RequestParser m) where
  liftIO io =
    RequestParser ((lift . lift . ExceptT . liftM (either (Left . fromString . show) Right) . liftIO . trySE) io)
    where
      trySE :: IO a -> IO (Either SomeException a)
      trySE =
        Strelka.Prelude.try

instance MonadTrans RequestParser where
  lift m =
    RequestParser (lift (lift (lift m)))

run :: RequestParser m a -> Request -> [Text] -> m (Either Text (a, [Text]))
run (RequestParser impl) request segments =
  runExceptT (runStateT (runReaderT impl request) segments)

fail :: Monad m => Text -> RequestParser m a
fail message =
  RequestParser $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

liftEither :: Monad m => Either Text a -> RequestParser m a
liftEither =
  RequestParser .
  lift .
  lift .
  ExceptT .
  return

-- |
-- 
liftMaybe :: Monad m => Maybe a -> RequestParser m a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right

-- |
-- Extract the error from RequestParser.
unliftEither :: Monad m => RequestParser m a -> RequestParser m (Either Text a)
unliftEither =
  tryError


-- * Path segments
-------------------------

-- |
-- Consume the next segment of the path.
consumeSegment :: Monad m => RequestParser m Text
consumeSegment =
  RequestParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

consumeSegmentWithParser :: Monad m => Q.Parser a -> RequestParser m a
consumeSegmentWithParser parser =
  consumeSegment >>= liftEither . first E.pack . Q.parseOnly parser

consumeSegmentIfIs :: Monad m => Text -> RequestParser m ()
consumeSegmentIfIs expectedSegment =
  do
    segment <- consumeSegment
    guard (segment == expectedSegment)

ensureThatNoSegmentsIsLeft :: Monad m => RequestParser m ()
ensureThatNoSegmentsIsLeft =
  RequestParser (lift (gets null)) >>= guard


-- * Methods
-------------------------

getMethod :: Monad m => RequestParser m ByteString
getMethod =
  do
    Request (Method method) _ _ _ _ <- RequestParser ask
    return method

ensureThatMethodIs :: Monad m => ByteString -> RequestParser m ()
ensureThatMethodIs expectedMethod =
  do
    method <- getMethod
    guard (expectedMethod == method)

-- |
-- Same as @ensureThatMethodIs "get"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsGet :: Monad m => RequestParser m ()
ensureThatMethodIsGet =
  ensureThatMethodIs "get"

-- |
-- Same as @ensureThatMethodIs "post"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsPost :: Monad m => RequestParser m ()
ensureThatMethodIsPost =
  ensureThatMethodIs "post"

-- |
-- Same as @ensureThatMethodIs "put"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsPut :: Monad m => RequestParser m ()
ensureThatMethodIsPut =
  ensureThatMethodIs "put"

-- |
-- Same as @ensureThatMethodIs "delete"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsDelete :: Monad m => RequestParser m ()
ensureThatMethodIsDelete =
  ensureThatMethodIs "delete"

-- |
-- Same as @ensureThatMethodIs "head"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsHead :: Monad m => RequestParser m ()
ensureThatMethodIsHead =
  ensureThatMethodIs "head"

-- |
-- Same as @ensureThatMethodIs "trace"@.
-- Exists for compile-time protection from typos.
ensureThatMethodIsTrace :: Monad m => RequestParser m ()
ensureThatMethodIsTrace =
  ensureThatMethodIs "trace"


-- * Headers
-------------------------

-- |
-- Lookup a header by name in lower-case.
getHeader :: Monad m => ByteString -> RequestParser m ByteString
getHeader name =
  do
    Request _ _ _ headers _ <- RequestParser ask
    liftMaybe (liftM (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

-- |
-- Ensure that the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
ensureThatAccepts :: Monad m => ByteString -> RequestParser m ()
ensureThatAccepts contentType =
  checkIfAccepts contentType >>=
  liftEither . bool (Left ("Unacceptable content-type: " <> fromString (show contentType))) (Right ())

-- |
-- Same as @ensureThatAccepts "text/plain"@.
-- Exists for compile-time protection from typos.
ensureThatAcceptsText :: Monad m => RequestParser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

-- |
-- Same as @ensureThatAccepts "text/html"@.
-- Exists for compile-time protection from typos.
ensureThatAcceptsHTML :: Monad m => RequestParser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

-- |
-- Same as @ensureThatAccepts "application/json"@.
-- Exists for compile-time protection from typos.
ensureThatAcceptsJSON :: Monad m => RequestParser m ()
ensureThatAcceptsJSON =
  ensureThatAccepts "application/json"

-- |
-- Check whether the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
checkIfAccepts :: Monad m => ByteString -> RequestParser m Bool
checkIfAccepts contentType =
  liftM (isJust . K.matchAccept [contentType]) (getHeader "accept")

-- |
-- Parse the username and password from the basic authorization header.
getAuthorization :: Monad m => RequestParser m (Text, Text)
getAuthorization =
  getHeader "authorization" >>= liftEither . D.basicCredentials


-- * Params
-------------------------

{- |
Get a parameter\'s value by its name, failing if the parameter is not present. 

@Maybe@ encodes whether the value was specified, i.e. @?name=value@ vs @?name@.
-}
getParam :: Monad m => ByteString -> RequestParser m (Maybe ByteString)
getParam name =
  do
    Request _ _ params _ _ <- RequestParser ask
    liftMaybe (liftM (\(ParamValue value) -> value) (G.lookup (ParamName name) params))


-- * Body
-------------------------

consumeBody :: MonadIO m => P.RequestBodyConsumer a -> RequestParser m a
consumeBody (P.RequestBodyConsumer consume) =
  do
    Request _ _ _ _ (InputStream getChunk) <- RequestParser ask
    liftIO (consume getChunk)

consumeBodyFolding :: MonadIO m => (a -> ByteString -> a) -> a -> RequestParser m a
consumeBodyFolding step init =
  consumeBody (P.foldBytes step init)

consumeBodyBuilding :: (MonadIO m, Monoid a) => (ByteString -> a) -> RequestParser m a
consumeBodyBuilding proj =
  consumeBody (P.build proj)

consumeBodyAsBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsBytes =
  consumeBody P.bytes

consumeBodyAsLazyBytes :: MonadIO m => RequestParser m B.ByteString
consumeBodyAsLazyBytes =
  consumeBody P.lazyBytes

consumeBodyAsBytesBuilder :: MonadIO m => RequestParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBody P.bytesBuilder

consumeBodyAsText :: MonadIO m => RequestParser m Text
consumeBodyAsText =
  consumeBody P.text

consumeBodyAsLazyText :: MonadIO m => RequestParser m L.Text
consumeBodyAsLazyText =
  consumeBody P.lazyText

consumeBodyAsTextBuilder :: MonadIO m => RequestParser m M.Builder
consumeBodyAsTextBuilder =
  consumeBody P.textBuilder

consumeBodyWithBytesParser :: MonadIO m => F.Parser a -> RequestParser m a
consumeBodyWithBytesParser parser =
  consumeBody (P.bytesParser parser) >>= liftEither

consumeBodyWithTextParser :: MonadIO m => Q.Parser a -> RequestParser m a
consumeBodyWithTextParser parser =
  consumeBody (P.textParser parser) >>= liftEither
