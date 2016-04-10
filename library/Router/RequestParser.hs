module Router.RequestParser where

import Router.Prelude
import qualified Network.Wai as Wai
import qualified Data.Attoparsec.Text
import qualified ByteString.TreeBuilder
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Router.Wai.Responses as Wai.Responses
import qualified Router.RequestProperties as RequestProperties


newtype RequestParser a =
  RequestParser (ReaderT RequestProperties.Properties (StateT [Text] (ExceptT Text IO)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)

instance Monoid (RequestParser a) where
  mempty =
    RequestParser empty
  mappend (RequestParser reader1) (RequestParser reader2) =
    RequestParser (reader1 <|> reader2)

requestParser :: Wai.Request -> RequestParser a -> IO (Either Text a)
requestParser request (RequestParser reader) =
  runExceptT $
  flip evalStateT (Wai.pathInfo request) $
  flip runReaderT (RequestProperties.fromRequest request) $
  reader

requestParserApplication :: RequestParser Wai.Response -> Wai.Application
requestParserApplication x =
  \request responseHandler ->
    requestParser request x >>= responseHandler . either (const Wai.Responses.notFound) id

failure :: Text -> RequestParser a
failure message =
  RequestParser $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

-- |
-- Consume the next segment of the path.
nextSegment :: RequestParser Text
nextSegment =
  RequestParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

method :: RequestParser Method
method =
  RequestParser $
  ReaderT $
  return . RequestProperties.method

param :: Text -> RequestParser (Maybe Text)
param name =
  RequestParser $
  ReaderT $
  \properties ->
    lift $
    ExceptT $
    return $
    maybe (Left ("Param \"" <> fromString (show name) <> "\" not found")) Right $
    RequestProperties.lookupParam name properties

consumeBody :: (IO ByteString -> IO () -> IO a) -> RequestParser a
consumeBody consumer =
  RequestParser $
  ReaderT $
  \properties ->
    lift $
    ExceptT $
    fmap (either (Left . fromString . show :: SomeException -> Either Text a) Right) $
    try $
    consumer (Wai.requestBody (RequestProperties.request properties)) (return ())

consumeBodyAsStrictBytes :: RequestParser ByteString
consumeBodyAsStrictBytes =
  consumeBody consumer
  where
    consumer nextChunk release =
      fmap ByteString.TreeBuilder.toByteString $
      loop mempty
      where
        loop acc =
          nextChunk >>= onChunk
          where
            onChunk chunk =
              if ByteString.null chunk
                then release $> acc
                else loop (acc <> ByteString.TreeBuilder.byteString chunk)

consumeBodyAsLazyBytes :: RequestParser ByteString.Lazy.ByteString
consumeBodyAsLazyBytes =
  RequestParser $
  ReaderT $
  \properties ->
    liftIO $
    Wai.strictRequestBody (RequestProperties.request properties)

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
  maybe (Left "") Right

