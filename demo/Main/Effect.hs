module Main.Effect where

import Rebase.Prelude
import Router.ResponseBuilder (ResponseBuilder)
import qualified Rebase.Data.HashSet as A


newtype Effect a =
  Effect (StateT (Numbers) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Numbers =
  HashSet Int

type Users =
  HashMap Text Text

addNumber :: Int -> Effect ()
addNumber x =
  Effect (modify (A.insert x))

listNumbers :: Effect [Int]
listNumbers =
  Effect (gets A.toList)

deleteNumber :: Int -> Effect ()
deleteNumber x =
  Effect (modify (A.delete x))

authorize :: Text -> Text -> Effect Bool
authorize username password =
  undefined

listCredentials :: Effect [(Text, Text)]
listCredentials =
  undefined
