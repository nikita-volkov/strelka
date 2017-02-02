{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParsing
(
  A.Params,
  A.param,
  C.lenientParam,
  -- * Multi-arity lenient param parser helpers
  C.lenientParams1,
  C.lenientParams2,
  C.lenientParams3,
  C.lenientParams4,
  C.lenientParams5,
  C.lenientParams6,
  C.lenientParams7,
  -- * Value parsers
  B.Value,
  B.parser,
  B.matcher,
  B.list,
  B.maybe,
  -- * Implicit lenient value parsers
  C.LenientValue(..),
)
where

import qualified Strelka.ParamsParsing.Params as A
import qualified Strelka.ParamsParsing.Value as B
import qualified Strelka.ParamsParsing.LenientValue as C
