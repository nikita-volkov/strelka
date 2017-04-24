{-# LANGUAGE CPP #-}
module Strelka.RequestBodyParsing.DefaultParser
where

import Strelka.Prelude
import qualified Strelka.RequestBodyParsing.Parser as A
import qualified Attoparsec.Data as B
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as D
import qualified Text.Builder as E
import qualified Data.ByteString.Lazy as F
import qualified Data.ByteString.Builder as G
import qualified ByteString.TreeBuilder as H


{-|
Provides a default request body parser.
-}
class DefaultParser a where
  defaultParser :: A.Parser a


-- * Generated instances
-------------------------

#define INSTANCE(TYPE, FUNCTION) instance DefaultParser TYPE where {{-# INLINE defaultParser #-}; defaultParser = FUNCTION;}
#define TEXT_PARSER_INSTANCE(TYPE) INSTANCE(TYPE, A.parseText B.lenientParser)

INSTANCE(Text, A.text)
INSTANCE(E.Builder, A.textBuilder)
INSTANCE(C.Text, A.lazyText)
INSTANCE(D.Builder, A.lazyTextBuilder)
INSTANCE(ByteString, A.bytes)
INSTANCE(H.Builder, A.bytesBuilder)
INSTANCE(F.ByteString, A.lazyBytes)
INSTANCE(G.Builder, A.lazyBytesBuilder)
TEXT_PARSER_INSTANCE(Char)
TEXT_PARSER_INSTANCE(Bool)
TEXT_PARSER_INSTANCE(Integer)
TEXT_PARSER_INSTANCE(Int)
TEXT_PARSER_INSTANCE(Int8)
TEXT_PARSER_INSTANCE(Int16)
TEXT_PARSER_INSTANCE(Int32)
TEXT_PARSER_INSTANCE(Int64)
TEXT_PARSER_INSTANCE(Word)
TEXT_PARSER_INSTANCE(Word8)
TEXT_PARSER_INSTANCE(Word16)
TEXT_PARSER_INSTANCE(Word32)
TEXT_PARSER_INSTANCE(Word64)
TEXT_PARSER_INSTANCE(Double)
TEXT_PARSER_INSTANCE(Scientific)
TEXT_PARSER_INSTANCE(TimeOfDay)
TEXT_PARSER_INSTANCE(Day)
TEXT_PARSER_INSTANCE(TimeZone)
TEXT_PARSER_INSTANCE(UTCTime)

#undef INSTANCE
#undef TEXT_PARSER_INSTANCE
