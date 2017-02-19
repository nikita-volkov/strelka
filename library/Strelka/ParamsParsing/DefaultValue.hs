{-# LANGUAGE CPP #-}
module Strelka.ParamsParsing.DefaultValue
where

import Strelka.Prelude
import qualified Attoparsec.Data.Implicit as B
import qualified Strelka.ParamsParsing.Value as D


{-|
Provides a default value parser.
-}
class DefaultValue value where
  defaultValue :: D.Value value

{-|
Uses 'D.maybe' over 'defaultValue'.
-}
instance DefaultValue a => DefaultValue (Maybe a) where
  {-# INLINE defaultValue #-}
  defaultValue =
    D.maybe defaultValue

{-|
Uses 'D.list' over 'defaultValue'.
-}
instance DefaultValue a => DefaultValue [a] where
  {-# INLINE defaultValue #-}
  defaultValue =
    D.list defaultValue


-- * Generated DefaultValue instances
-------------------------

#define INSTANCE1(TYPE, FUNCTION) instance DefaultValue TYPE where {{-# INLINE defaultValue #-}; defaultValue = FUNCTION;}
#define INSTANCE2(TYPE) INSTANCE1(TYPE, D.parser B.lenientParser)

INSTANCE1(Text, D.matcher Right)
INSTANCE1(String, D.string)
-- | Encodes the input using UTF8.
INSTANCE2(ByteString)
INSTANCE2(Char)
{-|
Interprets all the following inputs case-insensitively:
\"1\" or \"0\", \"true\" or \"false\", \"yes\" or \"no\", \"y\" or \"n\", \"t\" or \"f\".
The absense of a value is interpreted as 'False'.
-}
INSTANCE1(Bool, D.bool)
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

