module Router.Route where

import Router.Prelude
import Router.Model
import qualified Router.RequestHeadParser as A
import qualified Router.RequestBodyConsumer as B
import qualified Router.ResponseBuilder as C


type Route m =
  A.RequestHeadParser m (B.RequestBodyConsumer (m C.ResponseBuilder))



