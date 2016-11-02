module Main where

import Rebase.Prelude
import Router.RequestParser
import qualified Main.ResponseBuilders as A


main =
  undefined

routes =
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
      undefined
    notFound =
      ensureThatAccepts "text/html" *> html <|>
      ensureThatAccepts "application/json" *> json <|>
      text
      where
        html =
          undefined
        json =
          undefined
        text =
          undefined
