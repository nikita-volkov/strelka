module Router.RequestParser where

import Router.Prelude
import qualified Network.Wai as Wai
import qualified Data.Attoparsec.Text
import qualified Router.Wai.Responses as Wai.Responses


newtype RequestParser a =
  RequestParser (ReaderT Wai.Request (StateT [Text] (ExceptT Text IO)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)

instance Monoid (RequestParser a) where
  mempty =
    RequestParser empty
  mappend (RequestParser reader1) (RequestParser reader2) =
    RequestParser (reader1 <|> reader2)

requestParser :: Wai.Request -> RequestParser a -> IO (Either Text a)
requestParser request (RequestParser reader) =
  runReaderT reader request & \state -> evalStateT state (Wai.pathInfo request) & runExceptT

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
segment :: RequestParser Text
segment =
  RequestParser $
  lift $
  StateT $
  \case
    segmentsHead : segmentsTail ->
      return (segmentsHead, segmentsTail)
    _ ->
      ExceptT (return (Left "No segments left"))

segmentIs :: Text -> RequestParser ()
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
parseSegment :: Data.Attoparsec.Text.Parser a -> RequestParser a
parseSegment parser =
  segment >>=
  liftEither . either (Left . fromString) Right . Data.Attoparsec.Text.parseOnly parser

method :: RequestParser Method
method =
  undefined

liftEither :: Either Text a -> RequestParser a
liftEither =
  undefined

liftMaybe :: Maybe a -> RequestParser a
liftMaybe =
  undefined


