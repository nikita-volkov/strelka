module Strelka.ParamsParsing.Params
where

import Strelka.Prelude
import qualified Strelka.ParamsParsing.DefaultValue as A
import qualified Strelka.ParamsParsing.Value as C


{-|
Parser of a product of parameters.

Can be composed using the Applicative interface.
-}
newtype Params a =
  Params (ReaderT (Text -> Maybe [Text]) (Except Text) a)
  deriving (Functor, Applicative, Alternative)

run :: Params a -> (Text -> Maybe [Text]) -> Either Text a
run (Params reader) lookup =
  runExcept (runReaderT reader lookup)

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


-- * Default Helpers
-------------------------

-- | Parse a param by its name using the implicit default parser.
{-# INLINE defaultParam #-}
defaultParam :: A.DefaultValue a => Text -> Params a
defaultParam name =
  param name A.defaultValue

-- | Same as 'defaultParam'.
{-# INLINE defaultParams1 #-}
defaultParams1 :: A.DefaultValue a => Text -> Params a
defaultParams1 =
  defaultParam

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams2 #-}
defaultParams2 :: (A.DefaultValue a, A.DefaultValue b) => Text -> Text -> Params (a, b)
defaultParams2 name1 name2 =
  (,) <$>
  defaultParam name1 <*>
  defaultParam name2

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams3 #-}
defaultParams3 :: (A.DefaultValue a, A.DefaultValue b, A.DefaultValue c) => Text -> Text -> Text -> Params (a, b, c)
defaultParams3 name1 name2 name3 =
  (,,) <$>
  defaultParam name1 <*>
  defaultParam name2 <*>
  defaultParam name3

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams4 #-}
defaultParams4 :: (A.DefaultValue a, A.DefaultValue b, A.DefaultValue c, A.DefaultValue d) => Text -> Text -> Text -> Text -> Params (a, b, c, d)
defaultParams4 name1 name2 name3 name4 =
  (,,,) <$>
  defaultParam name1 <*>
  defaultParam name2 <*>
  defaultParam name3 <*>
  defaultParam name4

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams5 #-}
defaultParams5 :: (A.DefaultValue a, A.DefaultValue b, A.DefaultValue c, A.DefaultValue d, A.DefaultValue e) => Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e)
defaultParams5 name1 name2 name3 name4 name5 =
  (,,,,) <$>
  defaultParam name1 <*>
  defaultParam name2 <*>
  defaultParam name3 <*>
  defaultParam name4 <*>
  defaultParam name5

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams6 #-}
defaultParams6 :: (A.DefaultValue a, A.DefaultValue b, A.DefaultValue c, A.DefaultValue d, A.DefaultValue e, A.DefaultValue f) => Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f)
defaultParams6 name1 name2 name3 name4 name5 name6 =
  (,,,,,) <$>
  defaultParam name1 <*>
  defaultParam name2 <*>
  defaultParam name3 <*>
  defaultParam name4 <*>
  defaultParam name5 <*>
  defaultParam name6

-- | A helper abstracting over the Applicative composition of multiple 'defaultParam'.
{-# INLINE defaultParams7 #-}
defaultParams7 :: (A.DefaultValue a, A.DefaultValue b, A.DefaultValue c, A.DefaultValue d, A.DefaultValue e, A.DefaultValue f, A.DefaultValue g) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f, g)
defaultParams7 name1 name2 name3 name4 name5 name6 name7 =
  (,,,,,,) <$>
  defaultParam name1 <*>
  defaultParam name2 <*>
  defaultParam name3 <*>
  defaultParam name4 <*>
  defaultParam name5 <*>
  defaultParam name6 <*>
  defaultParam name7

