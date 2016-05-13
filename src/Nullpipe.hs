{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy         #-}
#endif

module Nullpipe
       where

import           Control.Monad   (forM_)
import           Data.ByteString (ByteString)
import           Nullpipe.Handle
import           Pipes
import           System.IO       (IOMode (..), openFile)

--nullpipe :: MonadIO m => FilePath -> Producer ByteString m
nullpipe fp = liftIO (openFile fp ReadMode) >>= go
  where go h = do
          r <- liftIO $ hGetTillNull h
          case r of
           Nothing -> return ()
           Just x -> yield x >> go h
