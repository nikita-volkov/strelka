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

failure :: Monad m => Text -> RequestParser m a
failure message =
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

liftMaybe :: Monad m => Maybe a -> RequestParser m a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right

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

consumeSegmentWithAttoparsec :: Monad m => Q.Parser a -> RequestParser m a
consumeSegmentWithAttoparsec parser =
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

ensureThatMethodIsGet :: Monad m => RequestParser m ()
ensureThatMethodIsGet =
  ensureThatMethodIs "get"

ensureThatMethodIsPost :: Monad m => RequestParser m ()
ensureThatMethodIsPost =
  ensureThatMethodIs "post"

ensureThatMethodIsPut :: Monad m => RequestParser m ()
ensureThatMethodIsPut =
  ensureThatMethodIs "put"

ensureThatMethodIsDelete :: Monad m => RequestParser m ()
ensureThatMethodIsDelete =
  ensureThatMethodIs "delete"


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

ensureThatAcceptsText :: Monad m => RequestParser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

ensureThatAcceptsHTML :: Monad m => RequestParser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

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

getAuthorization :: Monad m => RequestParser m (Text, Text)
getAuthorization =
  getHeader "authorization" >>= liftEither . D.basicCredentials


-- * Params
-------------------------

getParamAsText :: Text -> RequestParser m Text
getParamAsText name =
  undefined


-- * Body
-------------------------

getBody :: Monad m => RequestParser m InputStream
getBody =
  do
    Request _ _ _ _ x <- RequestParser ask
    return x

consumeBody :: MonadIO m => (IO ByteString -> IO a) -> RequestParser m a
consumeBody consume =
  do
    InputStream getChunk <- getBody
    liftIO (consume getChunk)

consumeBodyWithRequestBodyConsumer :: MonadIO m => P.RequestBodyConsumer a -> RequestParser m a
consumeBodyWithRequestBodyConsumer (P.RequestBodyConsumer consume) =
  consumeBody consume

consumeBodyFolding :: MonadIO m => (a -> ByteString -> a) -> a -> RequestParser m a
consumeBodyFolding step init =
  consumeBodyWithRequestBodyConsumer (P.foldBytes step init)

consumeBodyBuilding :: (MonadIO m, Monoid builder) => (ByteString -> builder) -> RequestParser m builder
consumeBodyBuilding proj =
  consumeBodyWithRequestBodyConsumer (P.building proj)

consumeBodyAsBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsBytes =
  consumeBodyWithRequestBodyConsumer P.bytes

consumeBodyAsLazyBytes :: MonadIO m => RequestParser m B.ByteString
consumeBodyAsLazyBytes =
  consumeBodyWithRequestBodyConsumer P.lazyBytes

consumeBodyAsBytesBuilder :: MonadIO m => RequestParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBodyWithRequestBodyConsumer P.bytesBuilder

consumeBodyAsText :: MonadIO m => RequestParser m Text
consumeBodyAsText =
  consumeBodyWithRequestBodyConsumer P.text

consumeBodyAsLazyText :: MonadIO m => RequestParser m L.Text
consumeBodyAsLazyText =
  consumeBodyWithRequestBodyConsumer P.lazyText

consumeBodyAsTextBuilder :: MonadIO m => RequestParser m M.Builder
consumeBodyAsTextBuilder =
  consumeBodyWithRequestBodyConsumer P.textBuilder

consumeBodyWithAttoparsec :: MonadIO m => F.Parser a -> RequestParser m a
consumeBodyWithAttoparsec parser =
  consumeBodyWithRequestBodyConsumer (P.attoparsecBytesParser parser) >>= liftEither
