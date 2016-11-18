module Router.RequestHeadParser where

import Router.Prelude
import Router.Model
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Builder as C
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as M
import qualified Data.Attoparsec.ByteString as F
import qualified Data.HashMap.Strict as G
import qualified Network.HTTP.Media as K
import qualified Router.RequestBodyConsumer as P
import qualified Router.HTTPAuthorizationParser as D
import qualified Router.ParamsParser as E


newtype RequestHeadParser m a =
  RequestHeadParser (ReaderT Request (StateT [Text] (ExceptT Text m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError Text)

instance MonadIO m => MonadIO (RequestHeadParser m) where
  liftIO io =
    RequestHeadParser ((lift . lift . ExceptT . fmap (either (Left . fromString . show) Right) . liftIO . trySE) io)
    where
      trySE :: IO a -> IO (Either SomeException a)
      trySE =
        Router.Prelude.try

instance MonadTrans RequestHeadParser where
  lift m =
    RequestHeadParser (lift (lift (lift m)))

run :: RequestHeadParser m a -> Request -> [Text] -> m (Either Text (a, [Text]))
run (RequestHeadParser impl) request segments =
  runExceptT (runStateT (runReaderT impl request) segments)

failure :: Monad m => Text -> RequestHeadParser m a
failure message =
  RequestHeadParser $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

liftEither :: Monad m => Either Text a -> RequestHeadParser m a
liftEither =
  RequestHeadParser .
  lift .
  lift .
  ExceptT .
  return

liftMaybe :: Monad m => Maybe a -> RequestHeadParser m a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right

unliftEither :: Monad m => RequestHeadParser m a -> RequestHeadParser m (Either Text a)
unliftEither =
  tryError


-- * Path segments
-------------------------

-- |
-- Consume the next segment of the path.
consumeSegment :: Monad m => RequestHeadParser m Text
consumeSegment =
  RequestHeadParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

consumeSegmentIfIs :: Monad m => Text -> RequestHeadParser m ()
consumeSegmentIfIs expectedSegment =
  do
    segment <- consumeSegment
    guard (segment == expectedSegment)

ensureThatNoSegmentsIsLeft :: Monad m => RequestHeadParser m ()
ensureThatNoSegmentsIsLeft =
  RequestHeadParser (lift (gets null)) >>= guard


-- * Methods
-------------------------

getMethod :: Monad m => RequestHeadParser m ByteString
getMethod =
  do
    Request (Method method) _ _ _ _ <- RequestHeadParser ask
    return method

ensureThatMethodIs :: Monad m => ByteString -> RequestHeadParser m ()
ensureThatMethodIs expectedMethod =
  do
    method <- getMethod
    guard (expectedMethod == method)

ensureThatMethodIsGet :: Monad m => RequestHeadParser m ()
ensureThatMethodIsGet =
  ensureThatMethodIs "get"

ensureThatMethodIsPost :: Monad m => RequestHeadParser m ()
ensureThatMethodIsPost =
  ensureThatMethodIs "post"

ensureThatMethodIsPut :: Monad m => RequestHeadParser m ()
ensureThatMethodIsPut =
  ensureThatMethodIs "put"

ensureThatMethodIsDelete :: Monad m => RequestHeadParser m ()
ensureThatMethodIsDelete =
  ensureThatMethodIs "delete"


-- * Headers
-------------------------

-- |
-- Lookup a header by name in lower-case.
getHeader :: Monad m => ByteString -> RequestHeadParser m ByteString
getHeader name =
  do
    Request _ _ _ headers _ <- RequestHeadParser ask
    liftMaybe (fmap (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

-- |
-- Ensure that the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
ensureThatAccepts :: Monad m => ByteString -> RequestHeadParser m ()
ensureThatAccepts contentType =
  checkIfAccepts contentType >>=
  liftEither . bool (Left ("Unacceptable content-type: " <> fromString (show contentType))) (Right ())

ensureThatAcceptsText :: Monad m => RequestHeadParser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

ensureThatAcceptsHTML :: Monad m => RequestHeadParser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

ensureThatAcceptsJSON :: Monad m => RequestHeadParser m ()
ensureThatAcceptsJSON =
  ensureThatAccepts "application/json"

-- |
-- Check whether the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
checkIfAccepts :: Monad m => ByteString -> RequestHeadParser m Bool
checkIfAccepts contentType =
  liftM (isJust . K.matchAccept [contentType]) (getHeader "accept")

getAuthorization :: Monad m => RequestHeadParser m (Text, Text)
getAuthorization =
  getHeader "authorization" >>= liftEither . D.basicCredentials


-- * Params
-------------------------

getParamAsText :: Text -> RequestHeadParser m Text
getParamAsText name =
  undefined


-- * Body
-------------------------

getBody :: Monad m => RequestHeadParser m InputStream
getBody =
  do
    Request _ _ _ _ x <- RequestHeadParser ask
    return x

consumeBody :: MonadIO m => (IO ByteString -> IO a) -> RequestHeadParser m a
consumeBody consume =
  do
    InputStream getChunk <- getBody
    liftIO (consume getChunk)

consumeBodyWithRequestBodyConsumer :: MonadIO m => P.RequestBodyConsumer a -> RequestHeadParser m a
consumeBodyWithRequestBodyConsumer (P.RequestBodyConsumer consume) =
  consumeBody consume

consumeBodyFolding :: MonadIO m => (a -> ByteString -> a) -> a -> RequestHeadParser m a
consumeBodyFolding step init =
  consumeBodyWithRequestBodyConsumer (P.folding step init)

consumeBodyBuilding :: (MonadIO m, Monoid builder) => (ByteString -> builder) -> RequestHeadParser m builder
consumeBodyBuilding proj =
  consumeBodyWithRequestBodyConsumer (P.building proj)

consumeBodyAsBytes :: MonadIO m => RequestHeadParser m ByteString
consumeBodyAsBytes =
  consumeBodyWithRequestBodyConsumer P.bytes

consumeBodyAsLazyBytes :: MonadIO m => RequestHeadParser m B.ByteString
consumeBodyAsLazyBytes =
  consumeBodyWithRequestBodyConsumer P.lazyBytes

consumeBodyAsBytesBuilder :: MonadIO m => RequestHeadParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBodyWithRequestBodyConsumer P.bytesBuilder

consumeBodyAsText :: MonadIO m => RequestHeadParser m Text
consumeBodyAsText =
  consumeBodyWithRequestBodyConsumer P.text

consumeBodyAsLazyText :: MonadIO m => RequestHeadParser m L.Text
consumeBodyAsLazyText =
  consumeBodyWithRequestBodyConsumer P.lazyText

consumeBodyAsTextBuilder :: MonadIO m => RequestHeadParser m M.Builder
consumeBodyAsTextBuilder =
  consumeBodyWithRequestBodyConsumer P.textBuilder

-- |
-- Consumes the input stream as an \"application/x-www-form-urlencoded\"
-- association list of parameters.
consumeBodyAsParams :: MonadIO m => E.ParamsParser a -> RequestHeadParser m a
consumeBodyAsParams paramsParser =
  consumeBodyWithRequestBodyConsumer (P.paramsParser paramsParser) >>= liftEither

consumeBodyWithAttoparsec :: MonadIO m => F.Parser a -> RequestHeadParser m a
consumeBodyWithAttoparsec parser =
  consumeBodyWithRequestBodyConsumer (P.attoparsecBytesParser parser) >>= liftEither
