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
      ensureThatAccepts "application/json" *> json <|>
      ensureThatAccepts "text/html" *> html
      where
        json =
          ensureThatMethodIs "get" *> get
          where
            get =
              error "list users in JSON"
        html =
          ensureThatMethodIs "get" *> get <|>
          ensureThatMethodIs "post" *> post
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
          ensureThatMethodIs "get" *> get <|>
          ensureThatMethodIs "post" *> post <|>
          ensureThatMethodIs "delete" *> delete
          where
            password =
              pure A.getPassword
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
      ensureThatAccepts "text/html" *> html <|>
      text
      where
        html =
          pure A.notFoundInHTML
        text =
          pure A.notFoundInText
