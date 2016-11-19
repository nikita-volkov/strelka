module Router.Route where

import Router.Prelude
import Router.Model
import qualified Router.RequestHeadParser as A
import qualified Router.RequestBodyConsumer as B
import qualified Router.ResponseBuilder as C
import qualified Data.Text as F
import qualified Data.Text.Encoding as D
import qualified Data.Text.Encoding.Error as E


type Route m =
  A.RequestHeadParser m (B.RequestBodyConsumer (m C.ResponseBuilder))

run :: MonadIO m => Route m -> Request -> m (Either Text Response)
run requestHeadParser request =
  do
    requestHeadParserResult <- A.run requestHeadParser request segments
    forM requestHeadParserResult $ \(B.RequestBodyConsumer consumeBody, _) -> do
      getResponseBuilder <- liftIO (consumeBody consumeBodyChunk)
      responseBuilder <- getResponseBuilder
      return (C.run responseBuilder)
  where
    Request _ (Path pathBytes) _ _ (InputStream consumeBodyChunk) =
      request
    segments =
      filter (not . F.null) (F.splitOn "/" (D.decodeUtf8With E.lenientDecode pathBytes))
