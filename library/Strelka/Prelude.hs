module Strelka.Prelude
( 
  module Exports,
  lowerCaseBytes_iso_8859_1,
  tryError,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (First(..), Last(..), (<>))

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (shift, callCC)
import Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT)
import Control.Monad.Trans.Maybe as Exports
import Control.Monad.Trans.Reader as Exports (Reader, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)

-- mtl
-------------------------
import Control.Monad.Cont.Class as Exports
import Control.Monad.Error.Class as Exports hiding (Error(..))
import Control.Monad.Reader.Class as Exports
import Control.Monad.State.Class as Exports
import Control.Monad.Writer.Class as Exports

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- bifunctors
-------------------------
import Data.Bifunctor as Exports

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- hashable
-------------------------
import Data.Hashable as Exports

-- Utils
-------------------------
import qualified Data.ByteString as ByteString

-- |
-- Lowercase according to ISO-8859-1.
lowerCaseBytes_iso_8859_1 :: ByteString -> ByteString
lowerCaseBytes_iso_8859_1 =
  ByteString.map byteTransformation
  where
    byteTransformation w =
      if transformable
        then w + 32
        else w
      where
        transformable =
          65 <= w && w <=  90 ||
          192 <= w && w <= 214 ||
          216 <= w && w <= 222

tryError :: MonadError e m => m a -> m (Either e a)
tryError m =
  catchError (liftM Right m) (return . Left)
