module Main.ResponseBuilders where

import Rebase.Prelude
import Router.ResponseBuilder


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  notFoundStatus <> html "<h1>404 Not Found</h1>"

notFoundInText :: ResponseBuilder
notFoundInText =
  notFoundStatus <> text "404 Not Found"

getPassword :: Text -> ResponseBuilder
getPassword username =
  undefined

putPassword :: Text -> ResponseBuilder
putPassword username =
  undefined

listUsersAsJSON :: [(Text, Text)] -> ResponseBuilder
listUsersAsJSON users =
  undefined

listUsersAsHTML :: [(Text, Text)] -> ResponseBuilder
listUsersAsHTML users =
  undefined
