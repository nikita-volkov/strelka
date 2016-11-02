module Router.RequestParser where

import Router.Prelude
import Router.Model
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified ByteString.TreeBuilder as C


newtype RequestParser a =
  RequestParser (ReaderT Request (StateT [Text] (ExceptT Text IO)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError Text)

instance MonadIO RequestParser where
  liftIO io =
    RequestParser ((lift . lift . ExceptT . fmap (either (Left . fromString . show) Right) . trySE) io)
    where
      trySE :: IO a -> IO (Either SomeException a)
      trySE =
        Router.Prelude.try


failure :: Text -> RequestParser a
failure message =
  RequestParser $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

try :: RequestParser a -> RequestParser (Either Text a)
try =
  tryError

liftEither :: Either Text a -> RequestParser a
liftEither =
  RequestParser .
  lift .
  lift .
  ExceptT .
  return

liftMaybe :: Maybe a -> RequestParser a
liftMaybe =
  liftEither .
  maybe (Left "Unexpected Nothing") Right


-- * Path segments
-------------------------

-- |
-- Consume the next segment of the path.
consumeSegment :: RequestParser Text
consumeSegment =
  RequestParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

consumeSegmentIfIs :: Text -> RequestParser ()
consumeSegmentIfIs expectedSegment =
  undefined

ensureThatNoSegmentsIsLeft :: RequestParser ()
ensureThatNoSegmentsIsLeft =
  undefined


-- * Methods
-------------------------

getMethod :: RequestParser ByteString
getMethod =
  do
    Request (Method method) _ _ _ _ <- RequestParser ask
    return method

ensureThatMethodIs :: ByteString -> RequestParser ()
ensureThatMethodIs expectedMethod =
  do
    method <- getMethod
    guard (expectedMethod == method)


-- * Headers
-------------------------

-- |
-- Lookup a header by name in lower-case.
getHeader :: ByteString -> RequestParser ByteString
getHeader name =
  undefined

-- |
-- Ensure that the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
ensureThatAccepts :: ByteString -> RequestParser ()
ensureThatAccepts contentType =
  undefined

-- |
-- Check whether the request provides an Accept header,
-- which includes the specified content type.
-- Content type must be in lower-case.
checkIfAccepts :: ByteString -> RequestParser Bool
checkIfAccepts contentType =
  undefined


-- * Params
-------------------------

getParam :: ByteString -> RequestParser ByteString
getParam name =
  undefined


-- * Body
-------------------------

consumeBody :: (IO ByteString -> IO a) -> RequestParser a
consumeBody consume =
  do
    Request _ _ _ _ (InputStream getChunk) <- RequestParser ask
    liftIO (consume getChunk)

consumeBodyAsBytesBuilder :: RequestParser C.Builder
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

consumeBodyAsStrictBytes :: RequestParser ByteString
consumeBodyAsStrictBytes =
  fmap C.toByteString consumeBodyAsBytesBuilder

consumeBodyAsLazyBytes :: RequestParser ByteString.Lazy.ByteString
consumeBodyAsLazyBytes =
  fmap C.toLazyByteString consumeBodyAsBytesBuilder


