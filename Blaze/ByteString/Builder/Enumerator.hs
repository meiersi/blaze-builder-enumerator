------------------------------------------------------------------------------
-- | 
-- Module       : Blaze.ByteString.Builder.Enumerator
-- License      : BSD3
-- Maintainer   : Thomas Sutton <me@thomas-sutton.id.au>
-- Stability    : Experimental
-- Portability  : Unknown
--
-- Simplify the process of using @blaze-builder@ with @enumerator@ by 
-- converting functions that construct 'Builder's into 'Iteratee's.
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Enumerator where

import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Enumerator

-- | Convert a function that returns a @blaze-builder@ 'Builder' into an 
-- 'Iteratee'. Each incoming value will be converted into a 'ByteString'.
iterBuilder :: Monad m => (a -> Builder) -> Iteratee a m B.ByteString
iterBuilder builder = continue (step builder)
 where
   step :: Monad m => (a -> Builder) -> Stream a -> Iteratee a m B.ByteString
   step b input = case input of
     EOF           -> yield (B.empty) EOF
     Chunks    []  -> continue (step b)
     Chunks (v:vs) -> yield (toByteString $ b v) (Chunks vs)
