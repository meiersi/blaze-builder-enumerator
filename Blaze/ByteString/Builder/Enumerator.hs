------------------------------------------------------------------------------
-- |
-- Module       : Blaze.ByteString.Builder.Enumerator
-- License      : BSD3
-- Maintainer   : Thomas Sutton <me@thomas-sutton.id.au>
-- Stability    : Experimental
-- Portability  : Unknown
--
-- Simplify the process of using @blaze-builder@ with @enumerator@ by
-- converting 'Builder's (and functions that constuct them) into
-- 'Enumerator's and 'Iteratee's.
--
-- Turning a 'Builder' into an 'Enumerator' is quite straightforward:
--
-- @
-- enumBuilder aBuilder $$ 'printChunks' True
-- @
--
-- Similarly, an composing a 'Builder' of all the input to an 'Iteratee' is
-- also straightforward:
--
-- @
-- 'iterHandle' stdin $$ iterBuilder
-- @
--
-- If you're processing streams of data you might want to use a construction
-- like the following to read some input from somewhere, parse
-- it into your data type, process each element, turn it back into text (or
-- bytes of any sort) and output it.
--
-- @
-- main = 'run_' $  'enumHandle' 1024 stdin
--             $$ 'sequence' (iterParser widgetParser)
--             $$ 'map' processWidget
--             $$ 'map' 
--             $$ 'iterHandle' stdout
-- @
--
------------------------------------------------------------------------------


module Blaze.ByteString.Builder.Enumerator where

import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as B
import           Data.Enumerator hiding (map)
import qualified Data.Enumerator as E
import           Data.Monoid
import           Prelude


-- | Create an 'Iteratee' which constucts a 'Builder' from its input.
iterBuilder :: Monad m => Iteratee B.ByteString m Builder
iterBuilder = continue $ step mempty
  where
    step :: Monad m => Builder -> Stream B.ByteString -> Iteratee B.ByteString m Builder
    step acc  EOF        = yield acc EOF
    step acc (Chunks []) = continue $ step acc
    step acc (Chunks ls) = continue $ step $ mconcat (acc:(map fromByteString ls))


concatBuilder :: Monad m => Iteratee Builder m Builder
concatBuilder = continue $ step mempty
  where
    step :: Monad m => Builder -> Stream Builder -> Iteratee Builder m Builder
    step acc  EOF        = yield acc EOF
    step acc (Chunks []) = continue $ step acc
    step acc (Chunks ls) = continue $ step $ mconcat (acc:ls)


-- | Create an 'Enumeratee' using the given function to translate values into
-- 'Builder's.
enumerateeBuilder :: Monad m => (input -> Builder) -> Enumeratee input Builder m b
enumerateeBuilder builder = E.map builder


-- | Create an 'Enumerator' from the ByteString produced by a 'Builder'.
enumBuilder :: Monad m => Builder -> Enumerator B.ByteString m b
enumBuilder builder step = case step of
    Yield x r  -> yield x r
    Continue k -> k $ Chunks [bs]
    Error err  -> throwError err
  where
      bs = toByteString builder
