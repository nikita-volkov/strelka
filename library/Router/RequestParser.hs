module Router.RequestParser where

import Router.Prelude
import Router.Model
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified ByteString.TreeBuilder as C
import qualified Router.HTTPAuthorizationParser as D
import qualified Router.ParamsParser as E


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

try :: Monad m => RequestParser m a -> RequestParser m (Either Text a)
try =
  tryError

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

consumeSegmentIfIs :: Text -> RequestParser m ()
consumeSegmentIfIs expectedSegment =
  undefined

ensureThatNoSegmentsIsLeft :: RequestParser m ()
ensureThatNoSegmentsIsLeft =
  undefined


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
getHeader :: ByteString -> RequestParser m ByteString
getHeader name =
  undefined

-- |
-- Ensure that the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
ensureThatAccepts :: ByteString -> RequestParser m ()
ensureThatAccepts contentType =
  undefined

ensureThatAcceptsText :: RequestParser m ()
ensureThatAcceptsText =
  ensureThatAccepts "text/plain"

ensureThatAcceptsHTML :: RequestParser m ()
ensureThatAcceptsHTML =
  ensureThatAccepts "text/html"

ensureThatAcceptsJSON :: RequestParser m ()
ensureThatAcceptsJSON =
  ensureThatAccepts "application/json"

-- |
-- Check whether the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
checkIfAccepts :: ByteString -> RequestParser m Bool
checkIfAccepts contentType =
  undefined

getAuthorization :: Monad m => RequestParser m (Text, Text)
getAuthorization =
  getHeader "Authorization" >>= liftEither . D.basicCredentials


-- * Params
-------------------------

getParamAsText :: Text -> RequestParser m Text
getParamAsText name =
  undefined


-- * Body
-------------------------

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
              if ByteString.null chunk
                then return acc
                else loop (acc <> C.byteString chunk)

consumeBodyAsStrictBytes :: MonadIO m => RequestParser m ByteString
consumeBodyAsStrictBytes =
  fmap C.toByteString consumeBodyAsBytesBuilder

consumeBodyAsLazyBytes :: MonadIO m => RequestParser m ByteString.Lazy.ByteString
consumeBodyAsLazyBytes =
  fmap C.toLazyByteString consumeBodyAsBytesBuilder

-- |
-- Consumes the input stream as an \"application/x-www-form-urlencoded\"
-- association list of parameters.
consumeBodyAsParams :: E.Params a -> RequestParser m a
consumeBodyAsParams =
  undefined
