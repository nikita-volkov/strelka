module Router.RequestParser where

import Router.Prelude
import qualified Network.Wai as Wai
import qualified Data.Attoparsec.Text
import qualified Router.Wai.Responses as Wai.Responses


newtype RequestParser m a =
  RequestParser (ReaderT Wai.Request (StateT [Text] (ExceptT Text m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

requestParser :: Monad m => Wai.Request -> RequestParser m a -> m (Either Text a)
requestParser request (RequestParser reader) =
  runReaderT reader request & \state -> evalStateT state (Wai.pathInfo request) & runExceptT

requestParserApplication :: Monad m => (forall a. m a -> IO a) -> RequestParser m Wai.Response -> Wai.Application
requestParserApplication mToIO x =
  \request responseHandler ->
    mToIO (requestParser request x) >>= responseHandler . either (const Wai.Responses.notFound) id

hoist :: (forall a. m1 a -> m2 a) -> (RequestParser m1 a -> RequestParser m2 a)
hoist transformation =
  undefined

failure :: Monad m => Text -> RequestParser m a
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
segment :: Monad m => RequestParser m Text
segment =
  RequestParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

segmentIs :: Monad m => Text -> RequestParser m ()
segmentIs expected =
  segment >>=
  \actual ->
    if actual == expected
      then
        return ()
      else
        failure $
        "The actual segment \"" <> actual <>
        "\" does not equal the expected \"" <> expected <> "\""

-- |
-- Consume the next path segment and parse it using Attoparsec.
parseSegment :: Monad m => Data.Attoparsec.Text.Parser a -> RequestParser m a
parseSegment parser =
  segment >>=
  liftEither . either (Left . fromString) Right . Data.Attoparsec.Text.parseOnly parser

method :: RequestParser m Method
method =
  undefined

liftEither :: Either Text a -> RequestParser m a
liftEither =
  undefined

liftMaybe :: Maybe a -> RequestParser m a
liftMaybe =
  undefined


