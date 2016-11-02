module Router.Route where

import Router.Prelude
import Router.Model
import qualified Router.RequestParser as A
import qualified Router.ResponseBuilder as B


type Route m =
  A.RequestParser m B.ResponseBuilder

run :: Monad m => Route m -> Request -> m (Either Text Response)
run route request =
  (liftM . liftM) (B.run . fst) (A.run route request segments)
  where
    segments =
      undefined
