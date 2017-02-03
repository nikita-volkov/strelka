# Intro

An HTTP server can be defined as a request parser, which produces a response, while managing the application state.
As simple as that.
This library exploits that fact to produce a very simple and flexible API, which can be executed on top of any specific HTTP-server implementation (e.g., Warp).

# The Why

## Composition

Haskell is famous for being best at parsing things. It's all due to its great abstractions like `Applicative`, `Alternative` and `Monad`, which let us approach the infinitely complex problems with decomposition. Yet for some reason the area of web-routing hasn't been approached that way much before.

So having composable parsers why do we have to downgrade to primitive uncomposable Ruby-ish patterns like the following?

```haskell
get "/user/:id" ...
```

Yet a question arises then: having already seen the textual parsers, which we all know and love, why can't we parse a request in the same fashion instead:

```haskell
do
  segmentIs "user"
  userID <- segment
  methodIsGet
  ...
```

## Type-safety

The uncomposable approach suffers from another problem: it's not type-checked.
Some libraries introduce TemplateHaskell or complex abstractions, even type-level programming to fix that, complicating the APIs.

The "Composable Request Parser" approach doesn't require any of that. Precise type-checking is already there.

## Performance

Libraries, which don't provide composable request parsers are bound to reparse the same things for each route. E.g.,

```haskell
get "/user/:id/profile" ...
get "/user/:id/status" ...
get "/user/:id/details" ...
```

In the example above to get to the third route, the request method needs to be matched three times and the `/user/:id/` part of the route needs to be reparsed.

None of that applies to a composable parser, which "strelka" provides.

## Simplicity

Web-routing is not rocket science. It doesn't necessarily require any advanced programming concepts like type-level programming with higher-kinded types. A router can be implemented using the familiar simple concepts.

# Demonstrations

## Helloworld

The following application shows how you can make a web server, which responds to the `/hi` and `/bye` routes with either HTML or plain text depending on what the user specifies in the "Accepts" request header.

You can clone and run the according project from [the "strelka-helloworld" repo](https://github.com/nikita-volkov/strelka-helloworld).

```haskell
module Main where

import Prelude
import qualified Strelka.RequestParsing as A
import qualified Strelka.ResponseBuilding as B
import qualified Strelka.WAI as C

main =
  C.strelkaServer 3000 (return . Right . runIdentity) route
  where
    route =
      A.segmentIs "hi" $> B.html "<h1>Hello world</h1>" <|>
      pure B.notFoundStatus
```

While this simple demo shows how easy it is to implement a simple web-server, there's too many things happening in the same module. This library was designed with separation of concerns in mind, and its every component facilitates that. So for a more correct and thorough demonstration its suggested to check out the following section.

## Thorough Demo

For a thorough demonstration of the library and suggested usage patterns
check out the following project:
<https://github.com/nikita-volkov/strelka-demo>.

# Known Integrations

* [WAI and Warp](http://hackage.haskell.org/package/strelka-wai)
