module Router.ParamsParser where

import Router.Prelude
import qualified ByteString.TreeBuilder as C
import qualified Router.HTTPAuthorizationParser as D
import qualified Ducers.Reducer as E
import qualified Data.Attoparsec.ByteString.Char8 as F


newtype ParamsParser a =
  ParamsParser (F.Parser a)
  deriving (Functor, Alternative)

instance Applicative ParamsParser where
  pure x =
    ParamsParser (pure x)
  (<*>) (ParamsParser atto1) (ParamsParser atto2) =
    ParamsParser (($) <$> atto1 <* F.char '&' <*> atto2)

param :: ByteString -> ParamParser a -> ParamsParser a
param name (ParamParser paramParser) =
  ParamsParser (paramsParser)
  where
    paramsParser =
      do
        F.string name
        F.char '='
        paramParser

paramByText :: Text -> ParamParser a -> ParamsParser a
paramByText =
  undefined


newtype ParamParser a =
  ParamParser (F.Parser a)
  deriving (Functor)

text :: ParamParser Text
text =
  undefined

int :: ParamParser Int
int =
  ParamParser (F.decimal <* ampersandOrEOI)
  where
    ampersandOrEOI =
      F.peekChar >>=
      \case
        Just '&' -> return ()
        Just _ -> fail "Unexpected input"
        Nothing -> return ()

customParamParser :: (ByteString -> Either Text a) -> ParamParser a
customParamParser =
  undefined
