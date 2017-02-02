module Strelka.ParamsParsing.Params
where

import Strelka.Prelude
import qualified Strelka.ParamsParsing.LenientValue as A
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


-- * Lenient Helpers
-------------------------

-- | Parse a param by its name using the implicit lenient parser.
{-# INLINE lenientParam #-}
lenientParam :: A.LenientValue a => Text -> Params a
lenientParam name =
  param name A.lenientValue

-- | Same as 'lenientParam'.
{-# INLINE lenientParams1 #-}
lenientParams1 :: A.LenientValue a => Text -> Params a
lenientParams1 =
  lenientParam

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams2 #-}
lenientParams2 :: (A.LenientValue a, A.LenientValue b) => Text -> Text -> Params (a, b)
lenientParams2 name1 name2 =
  (,) <$>
  lenientParam name1 <*>
  lenientParam name2

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams3 #-}
lenientParams3 :: (A.LenientValue a, A.LenientValue b, A.LenientValue c) => Text -> Text -> Text -> Params (a, b, c)
lenientParams3 name1 name2 name3 =
  (,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams4 #-}
lenientParams4 :: (A.LenientValue a, A.LenientValue b, A.LenientValue c, A.LenientValue d) => Text -> Text -> Text -> Text -> Params (a, b, c, d)
lenientParams4 name1 name2 name3 name4 =
  (,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams5 #-}
lenientParams5 :: (A.LenientValue a, A.LenientValue b, A.LenientValue c, A.LenientValue d, A.LenientValue e) => Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e)
lenientParams5 name1 name2 name3 name4 name5 =
  (,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams6 #-}
lenientParams6 :: (A.LenientValue a, A.LenientValue b, A.LenientValue c, A.LenientValue d, A.LenientValue e, A.LenientValue f) => Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f)
lenientParams6 name1 name2 name3 name4 name5 name6 =
  (,,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5 <*>
  lenientParam name6

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams7 #-}
lenientParams7 :: (A.LenientValue a, A.LenientValue b, A.LenientValue c, A.LenientValue d, A.LenientValue e, A.LenientValue f, A.LenientValue g) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Params (a, b, c, d, e, f, g)
lenientParams7 name1 name2 name3 name4 name5 name6 name7 =
  (,,,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5 <*>
  lenientParam name6 <*>
  lenientParam name7

