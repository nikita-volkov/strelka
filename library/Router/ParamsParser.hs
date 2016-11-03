module Router.ParamsParser where

import Router.Prelude
import qualified ByteString.TreeBuilder as C
import qualified Router.HTTPAuthorizationParser as D
import qualified Ducers.Reducer as E


newtype Params a =
  Params (E.Reducer IO ByteString (Either Text a))
  deriving (Functor)

instance Applicative Params where
  pure x =
    Params (pure (pure x))
  (<*>) (Params impl1) (Params impl2) =
    undefined

instance Alternative Params where

param :: ByteString -> Param a -> Params a
param name (Param paramReducer) =
  Params (paramsReducer)
  where
    paramsReducer =
      undefined

paramByText :: Text -> Param a -> Params a
paramByText =
  undefined


newtype Param a =
  Param (E.Reducer IO ByteString (Either Text a))
  deriving (Functor)

text :: Param Text
text =
  undefined

int :: Param Int
int =
  undefined

customParam :: (ByteString -> Either Text a) -> Param a
customParam =
  undefined
