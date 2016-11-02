module Main.Effect where

import Rebase.Prelude
import Router.ResponseBuilder (ResponseBuilder)
import qualified Rebase.Data.HashMap.Strict as A


newtype Effect a =
  Effect (StateT (HashMap Text Text) IO a)
  deriving (Functor, Applicative, Monad)

createUser :: Text -> Text -> Effect Bool
createUser name password =
  undefined

deleteUser :: Text -> Effect ()
deleteUser name =
  undefined

listUsers :: Effect [(Text, Text)]
listUsers =
  Effect (gets (A.toList))
