module Strelka.ParamsParsing.Value
where

import Strelka.Prelude hiding (maybe, list)
import qualified Data.Attoparsec.Text as G
import qualified Data.Text as E


{-|
A parser of a parameter value.
-}
newtype Value a =
  Value (ReaderT [Text] (Except Text) a)
  deriving (Functor, Applicative, Alternative)

{-|
Lifts an Attoparsec parser into Value.
-}
{-# INLINE parser #-}
parser :: G.Parser a -> Value a
parser parser =
  matcher (first fromString . G.parseOnly finishedParser)
  where
    finishedParser =
      parser <* (G.endOfInput <|> fail "Didn't parse the whole data")

{-|
Lifts a text-matching function into Value.
-}
{-# INLINE matcher #-}
matcher :: (Text -> Either Text a) -> Value a
matcher matcher =
  Value (ReaderT (except . join . liftM matcher . head))
  where
    head =
      \case
        x : _ -> Right x
        _ -> Left ("Not a single value is specified")

{-# INLINE text #-}
text :: Value Text
text =
  matcher Right

{-# INLINE string #-}
string :: Value String
string =
  matcher (Right . E.unpack)

{-|
Lifts a single value parser to the parser of a list of values.

Useful for decoding lists of values of the same name.
E.g., it'll work for the both following cases:

> ?param[]=1&param[]=2

> ?param=1&param=2

In both cases the name of the parameter to look up will be \"param\".
-}
{-# INLINE list #-}
list :: Value a -> Value [a]
list (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure)))

{-|
Lifts a single value parser to the parser of a possibly specified value.

It's useful for decoding the difference between the following two cases:

> ?param=value

> ?param
-}
{-# INLINE maybe #-}
maybe :: Value a -> Value (Maybe a)
maybe (Value (ReaderT singleValueFn)) =
  Value (ReaderT (traverse (singleValueFn . pure) . listToMaybe))

