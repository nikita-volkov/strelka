{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParsing
(
  A.Params,
  A.param,
  A.lenientParam,
  -- * Multi-arity lenient param parser helpers
  A.lenientParams1,
  A.lenientParams2,
  A.lenientParams3,
  A.lenientParams4,
  A.lenientParams5,
  A.lenientParams6,
  A.lenientParams7,
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
