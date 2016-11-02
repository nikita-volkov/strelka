module Main.RequestParsers where

import Rebase.Prelude
import Router.RequestParser
import Router.ResponseBuilder (ResponseBuilder)
import Main.Effect (Effect)
import qualified Main.Effect as B
import qualified Main.ResponseBuilders as A


top :: RequestParser B.Effect ResponseBuilder
top =
  consumeSegmentIfIs "users" *> users <|>
  consumeSegmentIfIs "user" *> user <|>
  notFound
  where
    users =
      ensureThatAcceptsJSON *> json <|>
      ensureThatAcceptsHTML *> html
      where
        json =
          ensureThatMethodIsGet *> get
          where
            get =
              lift (B.listUsersAsJSON)
        html =
          ensureThatMethodIsGet *> get <|>
          ensureThatMethodIsPost *> post
          where
            get =
              error "list users"
            post =
              do
                username <- getParamAsText "username"
                password <- getParamAsText "password"
                lift (B.createUserAsHTML username password)
    user =
      consumeSegment >>= onUsername
      where
        onUsername username =
          consumeSegmentIfIs "password" *> password <|>
          consumeSegmentIfIs "enabled" *> enabled <|>
          consumeSegmentIfIs "manage" *> manage <|>
          ensureThatMethodIsGet *> get <|>
          ensureThatMethodIsPost *> post <|>
          ensureThatMethodIsDelete *> delete
          where
            password =
              ensureThatMethodIsGet *> get <|>
              ensureThatMethodIsPut *> put
              where
                get =
                  undefined
                put =
                  undefined
            enabled =
              undefined
            manage =
              undefined
            get =
              undefined
            post =
              undefined
            delete =
              undefined
    notFound =
      ensureThatAcceptsHTML *> html <|>
      text
      where
        html =
          pure A.notFoundInHTML
        text =
          pure A.notFoundInText
