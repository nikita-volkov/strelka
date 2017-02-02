{-# LANGUAGE CPP #-}
module Strelka.ParamsParsing.LenientValue
where

import Strelka.Prelude
import qualified Attoparsec.Data.Implicit as B
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

