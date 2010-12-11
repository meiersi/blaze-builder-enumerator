{-# LANGUAGE BangPatterns #-}
------------------------------------------------------------------------------
-- |
-- Module       : Blaze.ByteString.Builder.Enumerator
-- Copyright    : (c) 2010 Simon Meier
-- License      : BSD3
--
-- Maintainer   : Simon Meier <iridcode@gmail.com>
-- Stability    : Experimental
-- Portability  : Tested on GHC only
--
-- Infrastructure and enumeratees for the incremental execution of builders and
-- passing on of the filled chunks as bytestrings to an inner iteratee.
--
-- Note that the @Buffer@ code is likely to move/change in order to
-- reconciliate it with the rest of the blaze-builder library.
--
------------------------------------------------------------------------------


module Blaze.ByteString.Builder.Enumerator (

  -- * Buffers
    Buffer

  -- ** Status information
  , freeSize 
  , sliceSize 
  , bufferSize 

  -- ** Creation and modification
  , allocBuffer 
  , reuseBuffer 
  , nextSlice 

  -- ** Conversion to bytestings
  , unsafeFreezeBuffer 
  , unsafeFreezeNonEmptyBuffer 

  -- * Buffer allocation strategies
  , BufferAllocStrategy
  , allNewBuffersStrategy 
  , reuseBufferStrategy 

  -- * Enumeratees from builders to bytestrings
  , builderToByteString 
  , unsafeBuilderToByteString 
  , builderToByteStringWith 

  ) where

import qualified Data.ByteString                   as S
import           Data.Enumerator      hiding (map)
import           Data.Monoid

import Control.Monad.IO.Class

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

------------------------------------------------------------------------------
-- Enumeratees for converting builders incrementally to bytestrings
------------------------------------------------------------------------------

-- Simple default instances
---------------------------

-- | Incrementally execute builders and pass on the filled chunks as
-- bytestrings.
builderToByteString :: MonadIO m => Enumeratee Builder S.ByteString m a
builderToByteString = 
  builderToByteStringWith (allNewBuffersStrategy defaultBufferSize)

-- | Incrementally execute builders on the given buffer and pass on the filled
-- chunks as bytestrings. Note that, if the given buffer is too small for the
-- execution of a build step, a larger one will be allocated.
--
-- WARNING: This enumeratee yields bytestrings that are NOT
-- referentially transparent. Their content will be overwritten as soon
-- as control is returned from the inner iteratee!
unsafeBuilderToByteString :: MonadIO m
                          => IO Buffer  -- action yielding the inital buffer.
                          -> Enumeratee Builder S.ByteString m a
unsafeBuilderToByteString = builderToByteStringWith . reuseBufferStrategy


-- | An enumeratee that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner iteratee.
--
-- INV: All bytestrings passed to the inner iteratee are non-empty.

--
-- based on the enumeratee code by Michael Snoyman <michael@snoyman.com>
--
builderToByteStringWith :: MonadIO m 
                        => BufferAllocStrategy
                        -> Enumeratee Builder S.ByteString m a
builderToByteStringWith (ioBuf0, nextBuf) step0 = do
    loop ioBuf0 step0
  where
    loop ioBuf = checkDone $ continue . step ioBuf

    step :: MonadIO m => IO (Buffer)
         -> (Stream S.ByteString -> Iteratee S.ByteString m b)
         -> Stream Builder
         -> Iteratee Builder m (Step S.ByteString m b)
    step ioBuf k EOF = do
        buf <- liftIO ioBuf
        case unsafeFreezeNonEmptyBuffer buf of
            Nothing -> yield (Continue k) EOF
            Just bs -> k (Chunks [bs]) >>== flip yield EOF
    step ioBuf k0 (Chunks xs) =
        go (unBuilder (mconcat xs) (buildStep finalStep)) ioBuf k0
      where
        finalStep !(BufRange pf _) = return $ Done pf ()

    go bStep ioBuf k = do
        !buf   <- liftIO ioBuf
        signal <- liftIO (execBuildStep bStep buf)
        case signal of
            Done op' _ -> continue $ step (return (updateEndOfSlice buf op')) k
            BufferFull minSize op' bStep' -> do
                let buf' = updateEndOfSlice buf op'
                    {-# INLINE cont #-}
                    cont k' = do
                        -- sequencing the computation of the next buffer
                        -- construction here ensures that the reference to the
                        -- foreign pointer `fp` is lost as soon as possible.
                        ioBuf' <- liftIO $ nextBuf minSize buf'
                        go bStep' ioBuf' k'
                case unsafeFreezeNonEmptyBuffer buf' of
                    Nothing -> cont k
                    Just bs ->
                        k (Chunks [bs]) >>== \step' ->
                            case step' of
                                Continue k' -> cont k'
                                _ -> return step' -- FIXME: Check that we don't loose any input here!
            InsertByteString op' bs bStep' -> do
                let buf' = updateEndOfSlice buf op'
                    bsk  = maybe id (:) $ unsafeFreezeNonEmptyBuffer buf'
                k (Chunks (bsk [bs])) >>== \step' ->
                    case step' of
                        Continue k' -> do
                            ioBuf' <- liftIO $ nextBuf 1 buf'
                            go bStep' ioBuf' k'
                        _ -> return step' -- FIXME: Check that we don't loose any input here!



{- Old testing code:
 
main :: IO ()
main = main1 >> main2 >> main3

main1 :: IO ()
main1 = do
    builder <- fromLazyByteString `fmap` L.readFile "test-input"
    withBinaryFile "test-output1" WriteMode $ \h -> run_ (go h builder)
  where
    go h builder = enumList 1 [builder]
      $$ joinI $ blaze
      $$ iterHandle h

main2 :: IO ()
main2 =
    withBinaryFile "test-output2" WriteMode $ \h -> run_ (go h)
  where
    go h = enumFile "test-input"
      $$ joinI $ E.map fromByteString
      $$ joinI $ blaze
      $$ iterHandle h

main3 :: IO ()
main3 =
    withBinaryFile "test-output3" WriteMode $ \h -> run_ (go h)
  where
    go h = enumList 1 (map S.singleton $ concat $ replicate 1000 [65..90])
      $$ joinI $ E.map (mconcat . map fromWord8 . S.unpack)
      $$ joinI $ blaze
      $$ iterHandle h

-}
