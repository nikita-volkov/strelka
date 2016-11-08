module Main.ResponseBuilders where

import Rebase.Prelude
import Router.ResponseBuilder
import qualified Rebase.Data.Text as A


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

listCredentialsAsJSON :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsJSON credentials =
  json (fromString bodyString)
  where
    bodyString =
      "[" <> foldMap credentialString credentials <> "]"
      where
        credentialString (username, password) =
          "{" <> "\"username\":" <> usernameString <> "," <> "\"password\":" <> passwordString <> "}"
          where
            usernameString =
              "\"" <> A.unpack username <> "\""
            passwordString =
              "\"" <> A.unpack password <> "\""

listCredentialsAsHTML :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsHTML credentials =
  html (fromString bodyString)
  where
    bodyString =
      "<ul>" <> foldMap credentialString credentials <> "</ul>"
      where
        credentialString (username, password) =
          "<li>" <> usernameString <> ":" <> passwordString <> "</li>"
          where
            usernameString =
              "<b>" <> A.unpack username <> "</b>"
            passwordString =
              A.unpack password

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
      fromString ("<ul>" <> foldMap (\x -> "<li>" <> show x <> "</li>") numbers <> "</ul>")

