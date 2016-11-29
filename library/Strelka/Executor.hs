module Strelka.Executor where

import Strelka.Prelude
import Strelka.Model
import qualified Strelka.RequestParser as A
import qualified Strelka.ResponseBuilder as B
import qualified Data.Text as C
import qualified Data.Text.Encoding as D
import qualified Data.Text.Encoding.Error as E


route :: Monad m => Request -> A.RequestParser m B.ResponseBuilder -> m (Either Text Response)
route request route =
  (liftM . liftM) (B.run . fst) (A.run route request segments)
  where
    segments =
      case request of
        Request _ (Path pathBytes) _ _ _ ->
          filter (not . C.null) (C.splitOn "/" (D.decodeUtf8With E.lenientDecode pathBytes))
