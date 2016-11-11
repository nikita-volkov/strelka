module Router.RequestParser where

import Router.Prelude
import Router.Model
import qualified Data.ByteString as A
import qualified Data.ByteString.Lazy as B
import qualified ByteString.TreeBuilder as C
import qualified Router.HTTPAuthorizationParser as D
import qualified Router.ParamsParser as E
import qualified Data.Attoparsec.ByteString as F
import qualified Data.HashMap.Strict as G
import qualified Ducers.Reducer as H
import qualified Ducers.Producer as I
import qualified Ducers.Attoparsec.Reducer as J
import qualified Network.HTTP.Media as K


newtype RequestParser m a =
  RequestParser (ReaderT Request (StateT [Text] (ExceptT Text m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError Text)

instance MonadIO m => MonadIO (RequestParser m) where
  liftIO io =
    RequestParser ((lift . lift . ExceptT . fmap (either (Left . fromString . show) Right) . liftIO . trySE) io)
    where
      trySE :: IO a -> IO (Either SomeException a)
      trySE =
        Router.Prelude.try

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
    liftMaybe (fmap (\(HeaderValue value) -> value) (G.lookup (HeaderName name) headers))

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

getBodyProducer :: Monad m => RequestParser m (I.Producer IO ByteString)
getBodyProducer =
  do
    Request _ _ _ _ (InputStream getChunk) <- RequestParser ask
    return (I.interruptibleAction (fmap (mfilter (not . A.null) . pure) getChunk))

consumeBody :: MonadIO m => (IO ByteString -> IO a) -> RequestParser m a
consumeBody consume =
  do
    Request _ _ _ _ (InputStream getChunk) <- RequestParser ask
    liftIO (consume getChunk)

consumeBodyAsBytesBuilder :: MonadIO m => RequestParser m C.Builder
consumeBodyAsBytesBuilder =
  consumeBody consumer
  where
    consumer getChunk =
      loop mempty
      where
        loop acc =
          getChunk >>= onChunk
          where
            onChunk chunk =
              if A.null chunk
                then return acc
                else loop (acc <> C.byteString chunk)

consumeBodyAsStrictBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsStrictBytes =
  fmap C.toByteString consumeBodyAsBytesBuilder

consumeBodyAsLazyBytes :: MonadIO m => RequestParser m B.ByteString
consumeBodyAsLazyBytes =
  fmap C.toLazyByteString consumeBodyAsBytesBuilder

-- |
-- Consumes the input stream as an \"application/x-www-form-urlencoded\"
-- association list of parameters.
consumeBodyAsParams :: MonadIO m => E.ParamsParser a -> RequestParser m a
consumeBodyAsParams (E.ParamsParser attoparsecParser) =
  consumeBodyWithAttoparsec attoparsecParser

consumeBodyWithAttoparsec :: MonadIO m => F.Parser a -> RequestParser m a
consumeBodyWithAttoparsec parser =
  consumeBodyWithReducer (J.bytesParser parser) >>= liftEither

consumeBodyWithReducer :: MonadIO m => H.Reducer IO ByteString a -> RequestParser m a
consumeBodyWithReducer reducer =
  getBodyProducer >>= \producer -> liftIO (I.runOnReducer producer reducer)
