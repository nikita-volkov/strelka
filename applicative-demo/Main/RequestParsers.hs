module Main.RequestParsers where

import Rebase.Prelude
import Router.RequestParser
import Router.ResponseBuilder (ResponseBuilder)
import qualified Main.ResponseBuilders as A


top :: RequestParser ResponseBuilder
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
              error "list users in JSON"
        html =
          ensureThatMethodIsGet *> get <|>
          ensureThatMethodIsPost *> post
          where
            get =
              error "list users"
            post =
              error "create a new user"
    user =
      ($) <$> responseFn <*> consumeSegment
      where
        responseFn =
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
                  pure A.getPassword
                put =
                  pure A.putPassword
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
