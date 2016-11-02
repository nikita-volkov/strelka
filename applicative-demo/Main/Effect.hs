module Main.Effect where

import Rebase.Prelude
import Router.ResponseBuilder (ResponseBuilder)
import qualified Main.ResponseBuilders as A
import qualified Rebase.Data.HashMap.Strict as B


newtype Effect a =
  Effect (StateT (HashMap Text Text) IO a)
  deriving (Functor, Applicative, Monad)

createUser :: Text -> Text -> Effect ()
createUser name password =
  undefined

deleteUser :: Text -> Effect ()
deleteUser name =
  undefined

createUserAsHTML :: Text -> Text -> Effect ResponseBuilder
createUserAsHTML name password =
  undefined

listUsersAsJSON :: Effect ResponseBuilder
listUsersAsJSON =
  Effect (gets (A.listUsersAsJSON . B.toList))

listUsersAsHTML :: Effect ResponseBuilder
listUsersAsHTML =
  Effect (gets (A.listUsersAsHTML . B.toList))

