module Router.Executor where

import Router.Prelude
import Router.Model
import qualified Router.RequestParser as A
import qualified Router.ResponseBuilder as B


route :: Monad m => Request -> A.RequestParser m B.ResponseBuilder -> m (Either Text Response)
route request route =
  (liftM . liftM) (B.run . fst) (A.run route request segments)
  where
    segments =
      undefined
