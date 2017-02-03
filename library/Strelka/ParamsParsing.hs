{-|
DSL for parsing of parameters.
-}
module Strelka.ParamsParsing
(
  A.Params,
  A.param,
  A.defaultParam,
  -- * Multi-arity default param parser helpers
  A.defaultParams1,
  A.defaultParams2,
  A.defaultParams3,
  A.defaultParams4,
  A.defaultParams5,
  A.defaultParams6,
  A.defaultParams7,
  -- * Value parsers
  B.Value,
  B.parser,
  B.matcher,
  B.list,
  B.maybe,
  -- * Implicit default value parsers
  C.DefaultValue(..),
)
where

import qualified Strelka.ParamsParsing.Params as A
import qualified Strelka.ParamsParsing.Value as B
import qualified Strelka.ParamsParsing.DefaultValue as C
