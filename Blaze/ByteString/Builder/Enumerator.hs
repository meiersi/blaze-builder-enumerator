------------------------------------------------------------------------------
-- |
-- Module       : Blaze.ByteString.Builder.Enumerator
-- License      : BSD3
-- Copyright    : 2010 Simon Meier <iridcode@gmail.com>
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


module Blaze.ByteString.Builder.Enumerator where

import Prelude
import Foreign

import qualified Data.ByteString                   as S
import qualified Data.ByteString.Internal          as S
import qualified Data.ByteString.Lazy              as L
import           Data.Enumerator      hiding (map)
import           Data.Monoid

import Control.Monad.IO.Class

import Blaze.ByteString.Builder.Internal

------------------------------------------------------------------------------
-- Buffers
------------------------------------------------------------------------------

-- | A buffer @Buffer fpbuf p0 op ope@ describes a buffer with the underlying
-- byte array @fpbuf..ope@, the currently written slice @p0..op@ and the free
-- space @op..ope@.
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
                     {-# UNPACK #-} !(Ptr Word8)        -- next free byte
                     {-# UNPACK #-} !(Ptr Word8)        -- first byte after buffer

-- | The size of the free space of the buffer.
freeSize :: Buffer -> Int
freeSize (Buffer _ _ op ope) = ope `minusPtr` op

-- | The size of the written slice in the buffer.
sliceSize :: Buffer -> Int
sliceSize (Buffer _ p0 op _) = op `minusPtr` p0

-- | The size of the whole byte array underlying the buffer.
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf _ _ ope) = 
    ope `minusPtr` unsafeForeignPtrToPtr fpbuf

-- | @allocBuffer size@ allocates a new buffer of size @size@.
{-# INLINE allocBuffer #-}
allocBuffer :: Int -> IO Buffer
allocBuffer size = do
    fpbuf <- S.mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf pbuf (pbuf `plusPtr` size)

-- | Resets the beginning of the next slice and the next free byte such that
-- the whole buffer can be filled again.
{-# INLINE reuseBuffer #-}
reuseBuffer :: Buffer -> Buffer
reuseBuffer (Buffer fpbuf _ _ ope) = Buffer fpbuf p0 p0 ope
  where
    p0 = unsafeForeignPtrToPtr fpbuf

-- | Convert the buffer to a bytestring. This operation is unsafe in the sense
-- that created bytestring shares the underlying byte array with the buffer.
-- Hence, depending on the later use of this buffer (e.g., if it gets reset and
-- filled again) referential transparency may be lost.
{-# INLINE unsafeFreezeBuffer #-}
unsafeFreezeBuffer :: Buffer -> S.ByteString
unsafeFreezeBuffer (Buffer fpbuf p0 op _) = 
    S.PS fpbuf (p0 `minusPtr` unsafeForeignPtrToPtr fpbuf) (op `minusPtr` p0)

-- | Convert a buffer to a non-empty bytestring. See 'unsafeFreezeBuffer' for
-- the explanation of why this operation may be unsafe.
{-# INLINE unsafeFreezeNonEmptyBuffer #-}
unsafeFreezeNonEmptyBuffer :: Buffer -> Maybe S.ByteString
unsafeFreezeNonEmptyBuffer buf
  | sliceSize buf <= 0 = Nothing
  | otherwise          = Just $ unsafeFreezeBuffer buf

-- | Update the end of slice pointer.
{-# INLINE updateEndOfSlice #-}
updateEndOfSlice :: Buffer    -- Old buffer
                 -> Ptr Word8 -- New end of slice  
                 -> Buffer    -- Updated buffer
updateEndOfSlice (Buffer fpbuf p0 _ ope) op' = Buffer fpbuf p0 op' ope

-- | Execute a build step on the given buffer.
{-# INLINE execBuildStep #-}
execBuildStep :: BuildStep
              -> Buffer  
              -> IO BuildSignal  
execBuildStep step (Buffer _ _ op ope) = step op ope

-- | Move the beginning of the slice to the next free byte such that the
-- remaining free space of the buffer can be filled further. This operation
-- is safe and can be used to fill the remaining part of the buffer after a
-- direct insertion of a bytestring or a flush.
{-# INLINE nextSlice #-}
nextSlice :: Int -> Buffer -> Maybe Buffer
nextSlice minSize (Buffer fpbuf _ op ope)
  | ope `minusPtr` op <= minSize = Nothing
  | otherwise                    = Just (Buffer fpbuf op op ope)

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


-- Buffer allocation strategies
-------------------------------

-- | A buffer allocation strategy @(buf0, nextBuf)@ specifies the initial
-- buffer to use and how to compute a new buffer @nextBuf minSize buf@ with at
-- least size @minSize@ from a filled buffer @buf@. The double nesting of the
-- @IO@ monad helps to ensure that the reference to the filled buffer @buf@ is
-- lost as soon as possible.
type BufferAllocStrategy = (IO Buffer, Int -> Buffer -> IO (IO Buffer))
  
-- | The simplest buffer allocation strategy: whenever a buffer is requested,
-- allocate a new one that is big enough for the next build step to execute.
--
-- NOTE that this allocation strategy may spill quite some memory upon direct
-- insertion of a bytestring by the builder. Thats no problem for garbage
-- collection, but it may lead to unreasonably high memory consumption in
-- special circumstances.
allNewBuffersStrategy :: Int                 -- Minimal buffer size.
                      -> BufferAllocStrategy
allNewBuffersStrategy bufSize = 
    ( allocBuffer bufSize
    , \reqSize _ -> return (allocBuffer (max reqSize bufSize)) )

-- | An unsafe, but possibly more efficient buffer allocation strategy:
-- reuse the buffer, if it is big enough for the next build step to execute.
reuseBufferStrategy :: IO Buffer          
                    -> BufferAllocStrategy
reuseBufferStrategy buf0 =
    (buf0, tryReuseBuffer)
  where
    tryReuseBuffer reqSize buf
      | bufferSize buf >= reqSize = return $ return (reuseBuffer buf)
      | otherwise                 = return $ allocBuffer reqSize

-- | An enumeratee that incrementally executes builders and passes on the
-- filled chunks as bytestrings to an inner iteratee.
--
-- INV: All bytestrings passed to the inner iteratee are non-empty.
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
        go (unBuilder (mconcat xs) finalStep) ioBuf k0
      where
        unBuilder (Builder x) = x
        finalStep pf _ = return $ Done pf

    go buildStep ioBuf k = do
        buf    <- liftIO ioBuf
        signal <- liftIO (execBuildStep buildStep buf)
        case signal of
            Done op' -> continue $ step (return (updateEndOfSlice buf op')) k
            BufferFull minSize op' buildStep' -> do
                let buf' = updateEndOfSlice buf op'
                    {-# INLINE cont #-}
                    cont k' = do
                        -- sequencing the computation of the next buffer
                        -- construction here ensures that the reference to the
                        -- foreign pointer `fp` is lost as soon as possible.
                        ioBuf' <- liftIO $ nextBuf minSize buf'
                        go buildStep' ioBuf' k'
                case unsafeFreezeNonEmptyBuffer buf' of
                    Nothing -> cont k
                    Just bs ->
                        k (Chunks [bs]) >>== \step' ->
                            case step' of
                                Continue k' -> cont k'
                                _ -> return step' -- FIXME: Check that we don't loose any input here!
            ModifyChunks op' lbss buildStep' -> do
                let buf' = updateEndOfSlice buf op'
                    bsk  = maybe id (:) $ unsafeFreezeNonEmptyBuffer buf'
                    bss = bsk . L.toChunks $ lbss L.empty
                k (Chunks bss) >>== \step' ->
                    case step' of
                        Continue k' -> do
                            ioBuf' <- liftIO $ nextBuf 1 buf'
                            go buildStep' ioBuf' k'
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
