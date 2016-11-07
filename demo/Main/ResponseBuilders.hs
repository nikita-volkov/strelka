module Main.ResponseBuilders where

import Rebase.Prelude
import Router.ResponseBuilder


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  notFoundStatus <> html "<h1>404 Not Found</h1>"

notFoundInText :: ResponseBuilder
notFoundInText =
  notFoundStatus <> text "404 Not Found"

badRequestInHTML :: ResponseBuilder
badRequestInHTML =
  badRequestStatus <> html "<h1>400 Bad Request</h1>"

badRequestInText :: ResponseBuilder
badRequestInText =
  badRequestStatus <> text "400 Bad Request"

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

listNumbersAsJSON :: [Int] -> ResponseBuilder
listNumbersAsJSON numbers =
  json body
  where
    body =
      fromString ("[" <> intercalate "," (map show numbers) <> "]")

listNumbersAsHTML :: [Int] -> ResponseBuilder
listNumbersAsHTML numbers =
  html body
  where
    body =
      fromString (foldMap (\x -> "<p>" <> show x <> "</p>") numbers)
