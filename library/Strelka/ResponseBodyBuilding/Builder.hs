module Strelka.ResponseBodyBuilding.Builder where

import Strelka.Prelude
import qualified Data.ByteString as C
import qualified Data.ByteString.Lazy as D
import qualified Data.ByteString.Builder as E
import qualified Data.Text.Encoding as H
import qualified Data.Text.Lazy as F
import qualified Data.Text.Lazy.Encoding as I
import qualified Data.Text.Lazy.Builder as J


{-|
A builder of the response body.
-}
newtype Builder =
  Builder ((ByteString -> IO ()) -> IO () -> IO ())

instance IsString Builder where
  fromString string =
    bytesBuilder (E.stringUtf8 string)

instance Monoid Builder where
  mempty =
    Builder (\_ flush -> flush)
  mappend (Builder cont1) (Builder cont2) =
    Builder (\feed flush -> cont1 feed (pure ()) *> cont2 feed flush)

instance Semigroup Builder


{-|
Lift ByteString.
-}
bytes :: ByteString -> Builder
bytes x =
  Builder (\feed flush -> feed x *> flush)

{-|
Lift lazy ByteString.
-}
lazyBytes :: D.ByteString -> Builder
lazyBytes x =
  Builder (\feed flush -> D.foldlChunks (\io chunk -> io >> feed chunk) (pure ()) x >> flush)

{-|
Lift ByteString Builder.
-}
bytesBuilder :: E.Builder -> Builder
bytesBuilder =
  lazyBytes . E.toLazyByteString

{-|
Lift Text.
-}
text :: Text -> Builder
text text =
  bytes (H.encodeUtf8 text)

{-|
Lift lazy Text.
-}
lazyText :: F.Text -> Builder
lazyText text =
  Builder impl
  where
    impl feed flush =
      F.foldlChunks step (pure ()) text *> flush
      where
        step io textChunk =
          io *> feed bytesChunk
          where
            bytesChunk =
              H.encodeUtf8 textChunk

{-|
Lift ByteString Builder.
-}
textBuilder :: J.Builder -> Builder
textBuilder =
  lazyText . J.toLazyText

{-|
Lift a handler of the feeding and flushing actions.
-}
actions :: ((ByteString -> IO ()) -> IO () -> IO ()) -> Builder
actions =
  Builder
