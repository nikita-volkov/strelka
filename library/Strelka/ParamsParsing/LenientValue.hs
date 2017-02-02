{-# LANGUAGE CPP #-}
module Strelka.ParamsParsing.LenientValue
where

import Strelka.Prelude
import qualified Attoparsec.Data.Implicit as B
import qualified Strelka.ParamsParsing.Params as C
import qualified Strelka.ParamsParsing.Value as D


{-|
Provides a default lenient value parser.
-}
class LenientValue value where
  lenientValue :: D.Value value

{-|
Uses 'D.maybe' over 'lenientValue'.
-}
instance LenientValue a => LenientValue (Maybe a) where
  {-# INLINE lenientValue #-}
  lenientValue =
    D.maybe lenientValue

{-|
Uses 'D.list' over 'lenientValue'.
-}
instance LenientValue a => LenientValue [a] where
  {-# INLINE lenientValue #-}
  lenientValue =
    D.list lenientValue


-- * Generated LenientValue instances
-------------------------

#define INSTANCE1(TYPE, FUNCTION) instance LenientValue TYPE where {{-# INLINE lenientValue #-}; lenientValue = FUNCTION;}
#define INSTANCE2(TYPE) INSTANCE1(TYPE, D.parser B.lenientParser)

INSTANCE1(Text, D.matcher Right)
INSTANCE1(String, D.string)
-- | Encodes the input using UTF8.
INSTANCE2(ByteString)
INSTANCE2(Char)
INSTANCE2(Bool)
INSTANCE2(Integer)
INSTANCE2(Int)
INSTANCE2(Int8)
INSTANCE2(Int16)
INSTANCE2(Int32)
INSTANCE2(Int64)
INSTANCE2(Word)
INSTANCE2(Word8)
INSTANCE2(Word16)
INSTANCE2(Word32)
INSTANCE2(Word64)
INSTANCE2(Double)
INSTANCE2(Scientific)
INSTANCE2(TimeOfDay)
INSTANCE2(Day)
INSTANCE2(TimeZone)
INSTANCE2(UTCTime)

#undef INSTANCE1
#undef INSTANCE2


-- * Params helpers
-------------------------

-- | Parse a param by its name using the implicit lenient parser.
{-# INLINE lenientParam #-}
lenientParam :: LenientValue a => Text -> C.Params a
lenientParam name =
  C.param name lenientValue

-- | Same as 'lenientParam'.
{-# INLINE lenientParams1 #-}
lenientParams1 :: LenientValue a => Text -> C.Params a
lenientParams1 =
  lenientParam

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams2 #-}
lenientParams2 :: (LenientValue a, LenientValue b) => Text -> Text -> C.Params (a, b)
lenientParams2 name1 name2 =
  (,) <$>
  lenientParam name1 <*>
  lenientParam name2

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams3 #-}
lenientParams3 :: (LenientValue a, LenientValue b, LenientValue c) => Text -> Text -> Text -> C.Params (a, b, c)
lenientParams3 name1 name2 name3 =
  (,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams4 #-}
lenientParams4 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d) => Text -> Text -> Text -> Text -> C.Params (a, b, c, d)
lenientParams4 name1 name2 name3 name4 =
  (,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams5 #-}
lenientParams5 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e) => Text -> Text -> Text -> Text -> Text -> C.Params (a, b, c, d, e)
lenientParams5 name1 name2 name3 name4 name5 =
  (,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5

-- | A helper abstracting over the Applicative composition of multiple 'lenientParam'.
{-# INLINE lenientParams6 #-}
lenientParams6 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e, LenientValue f) => Text -> Text -> Text -> Text -> Text -> Text -> C.Params (a, b, c, d, e, f)
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
lenientParams7 :: (LenientValue a, LenientValue b, LenientValue c, LenientValue d, LenientValue e, LenientValue f, LenientValue g) => Text -> Text -> Text -> Text -> Text -> Text -> Text -> C.Params (a, b, c, d, e, f, g)
lenientParams7 name1 name2 name3 name4 name5 name6 name7 =
  (,,,,,,) <$>
  lenientParam name1 <*>
  lenientParam name2 <*>
  lenientParam name3 <*>
  lenientParam name4 <*>
  lenientParam name5 <*>
  lenientParam name6 <*>
  lenientParam name7

