module Router.Wai.Responses where

import Router.Prelude
import qualified Network.Wai.Internal as Wai
import qualified Network.HTTP.Types.Status as HTTP.Types.Status


notFound :: Response
notFound =
  Wai.ResponseStream HTTP.Types.Status.status404 [] undefined

file :: FilePath -> Response
file path =
  undefined

