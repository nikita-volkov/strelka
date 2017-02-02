module Strelka.ParamsParsing.Params
where

import Strelka.Prelude
import qualified Strelka.ParamsParsing.Value as C


{-|
Parser of a product of parameters.

Can be composed using the Applicative interface.
-}
newtype Params a =
  Params (ReaderT (Text -> Maybe [Text]) (Except Text) a)
  deriving (Functor, Applicative, Alternative)

-- | Parse a param by its name using an explicit parser.
{-# INLINABLE param #-}
param :: Text -> C.Value a -> Params a
param name value =
  Params (ReaderT onLookup)
  where
    onLookup lookup =
      maybe notFound found (lookup name)
      where
        notFound =
          (except . Left) ("Parameter \"" <> name <> "\" not found")
        found input =
          case value of
            C.Value (ReaderT onInput) -> withExcept updateError (onInput input)
          where
            updateError x =
              "Parameter \"" <> name <> "\" values parsing failure: " <> x
