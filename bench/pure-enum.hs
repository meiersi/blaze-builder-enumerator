{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Blaze.ByteString.Builder.Enumerator
import Data.Monoid
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad (foldM)
import Data.List (foldl')

row = mconcat
    [ fromByteString "<tr>"
    , mconcat $ map go [1..100]
    , fromByteString "</tr>"
    ]
  where
    go i = mconcat
        [ fromByteString "<td>"
        , fromString $ show i
        , fromByteString "</td>"
        ]

table = mconcat
    [ fromByteString "<table>"
    , mconcat $ replicate 1000 row
    , fromByteString "</table>"
    ]

main = defaultMain
    [ bgroup "ioref"
        [ bench "pure" $ do
            x <- newIORef (0 :: Int)
            let go bs = do
                    x' <- readIORef x
                    writeIORef x $ x' + S.length bs
            mapM_ go $ L.toChunks $ toLazyByteString table
            readIORef x
        , bench "enumeratee" $ do
            x <- newIORef (0 :: Int)
            let iter = do
                    mbs <- E.head
                    case mbs of
                        Nothing -> return ()
                        Just bs -> do
                            x' <- liftIO $ readIORef x
                            liftIO $ writeIORef x $ x' + S.length bs
                            iter
            E.run_ $ E.enumList 1 [table] $$ E.joinI
                   $ builderToByteString $$ iter
            readIORef x
        ]
    , bgroup "accum"
        [ let go i bs = i + S.length bs
           in bench "pure" $ flip nf table $ foldl' go 0 . L.toChunks . toLazyByteString
        , bench "enumeratee" $ do
            let go i bs = i + S.length bs
            E.run_ $ E.enumList 1 [table] $$ E.joinI
                   $ builderToByteString $$ E.liftFoldL' go 0 :: IO Int
        ]
    ]
