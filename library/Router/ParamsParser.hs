module Router.ParamsParser where

import Router.Prelude
import qualified ByteString.TreeBuilder as C
import qualified Router.HTTPAuthorizationParser as D


newtype Params a =
  Params (IO ByteString -> IO (Either Text a))
  deriving (Functor)

instance Applicative Params where
  pure x =
    Params (const (pure (pure x)))
  (<*>) (Params impl1) (Params impl2) =
    undefined

instance Alternative Params where

param :: ByteString -> Param a -> Params a
param =
  undefined


newtype Param a =
  Param (IO ByteString -> IO (Either Text a))
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
