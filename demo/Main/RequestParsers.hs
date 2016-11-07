module Main.RequestParsers where

import Rebase.Prelude
import Router.RequestParser
import Router.ParamsParser
import Router.ResponseBuilder (ResponseBuilder)
import Main.Effect (Effect)
import qualified Main.Effect as B
import qualified Main.ResponseBuilders as A
import qualified Router.ResponseBuilder as C
import qualified Data.Attoparsec.ByteString.Char8 as D


type Route =
  RequestParser B.Effect ResponseBuilder

top :: Route
top =
  consumeSegmentIfIs "numbers" *> numbers <|>
  consumeSegmentIfIs "users" *> users <|>
  notFound
  where
    numbers =
      ensureThatMethodIsGet *> get <|>
      ensureThatMethodIsPut *> put <|>
      ensureThatMethodIsDelete *> delete
      where
        get =
          ensureThatAcceptsJSON *> json <|>
          ensureThatAcceptsHTML *> html
          where
            json =
              A.listNumbersAsJSON <$> lift B.listNumbers
            html =
              A.listNumbersAsHTML <$> lift B.listNumbers
        put =
          authorizing "" authorized
          where
            authorized =
              consumingBodyAsInt onInt
              where
                onInt int =
                  lift (B.addNumber int) *> okay
        delete =
          authorizing "" authorized
          where
            authorized =
              consumingBodyAsInt onInt
              where
                onInt int =
                  lift (B.deleteNumber int) *> okay
    users =
      undefined
    notFound =
      ensureThatAcceptsHTML *> html <|>
      text
      where
        html =
          pure A.notFoundInHTML
        text =
          pure A.notFoundInText

-- |
-- Reusable route for wrapping other routes with HTTP Authorization.
authorizing :: ByteString -> Route -> Route
authorizing realm authorized =
  authorize *> authorized <|>
  unauthorized
  where
    authorize =
      do
        (username, password) <- getAuthorization
        success <- lift (B.authorize username password)
        guard success
    unauthorized =
      pure (C.unauthorized realm)

okay :: Route
okay =
  pure C.okayStatus

badRequest :: Route
badRequest =
  ensureThatAcceptsHTML *> pure A.badRequestInHTML <|>
  ensureThatAcceptsText *> pure A.badRequestInText <|>
  pure C.badRequestStatus

consumingBodyAsInt :: (Int -> Route) -> Route
consumingBodyAsInt onInt =
  do
    parsingResult <- consumeBodyWithAttoparsec (D.decimal <* D.endOfInput)
    either (const badRequest) onInt parsingResult
