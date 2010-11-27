------------------------------------------------------------------------------
-- | 
-- Module       : Blaze.ByteString.Builder.Enumerator
-- License      : BSD3
-- Maintainer   : Thomas Sutton <me@thomas-sutton.id.au>
-- Stability    : Experimental
-- Portability  : Unknown
--
-- Wrap 
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Enumerator where

import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Enumerator

-- | Convert a function that returns a @blaze-builder@ 'Builder' into an 
-- 'Iteratee'. Each incoming value will be converted into a lazy 'ByteString'.
iterBuilder :: Monad m => (a -> Builder) -> Iteratee a m B.ByteString
iterBuilder build = continue (step build)

step :: Monad m => (a -> Builder) -> Stream a -> Iteratee a m B.ByteString
step build input = case input of
  EOF           -> yield (B.empty) EOF
  Chunks    []  -> continue (step build)
  Chunks (v:vs) -> yield (toByteString $ build v) (Chunks vs)
