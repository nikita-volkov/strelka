module Main where

import Rebase.Prelude hiding (try)
import Router.RequestParser


main =
  undefined

routes =
  do
    segment <- consumeSegment
    case segment of
      "users" -> users
      "user" -> user
      _ -> mzero
  where
    users =
      do
        acceptsJSON <- checkIfAccepts "application/json"
        if acceptsJSON
          then json
          else do
            acceptsHTML <- checkIfAccepts "text/html"
            if acceptsHTML
              then html
              else mzero
      where
        json =
          do
            method <- getMethod
            if method == "get"
              then undefined
              else failure ("Method \"" <> (fromString . show) method <> "\" is not supported")
        html =
          do
            method <- getMethod
            case method of
              "get" -> get
              "post" -> post
              _ -> mzero
          where
            get =
              error "list users"
            post =
              error "create a new user"
    user =
      consumeSegment >>= onUsername
      where
        onUsername username =
          do
            segmentEither <- try consumeSegment
            case segmentEither of
              Right segment ->
                case segment of
                  "password" -> password
                  "enabled" -> enabled
                  "manage" -> manage
                  _ -> mzero
              Left _ -> do
                method <- getMethod
                case method of
                  "get" -> get
                  "put" -> put
                  "delete" -> delete
                  _ -> mzero
          where
            password =
              getMethod >>=
              \case
                "get" -> get
                "put" -> put
                _ -> mzero
              where
                get =
                  do
                    guard =<< checkIfAccepts "application/html"
                    undefined
                put =
                  undefined
            enabled =
              undefined
            manage =
              undefined
            get =
              undefined
            put =
              undefined
            delete =
              undefined
