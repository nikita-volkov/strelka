module Router where

import Router.Prelude
import qualified Network.Wai as Wai
import qualified Data.Attoparsec.Text
import qualified Router.Wai.Responses as Wai.Responses


newtype Route m a =
  Route (ReaderT Wai.Request (StateT [Text] (ExceptT Text m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

route :: Monad m => Wai.Request -> Route m a -> m (Either Text a)
route request (Route reader) =
  runReaderT reader request & \state -> evalStateT state (Wai.pathInfo request) & runExceptT

routeApplication :: Monad m => (forall a. m a -> IO a) -> Route m Wai.Response -> Wai.Application
routeApplication mToIO x =
  \request responseHandler ->
    mToIO (route request x) >>= responseHandler . either (const Wai.Responses.notFound) id

hoist :: (forall a. m1 a -> m2 a) -> (Route m1 a -> Route m2 a)
hoist transformation =
  undefined

failure :: Monad m => Text -> Route m a
failure message =
  Route $
  lift $
  lift $
  ExceptT $
  return $
  Left $
  message

-- |
-- Consume the next segment of the path.
segment :: Monad m => Route m Text
segment =
  Route $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

segmentIs :: Monad m => Text -> Route m ()
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
parseSegment :: Monad m => Data.Attoparsec.Text.Parser a -> Route m a
parseSegment parser =
  segment >>=
  liftEither . either (Left . fromString) Right . Data.Attoparsec.Text.parseOnly parser

method :: Route m Method
method =
  undefined

liftEither :: Either Text a -> Route m a
liftEither =
  undefined

liftMaybe :: Maybe a -> Route m a
liftMaybe =
  undefined


