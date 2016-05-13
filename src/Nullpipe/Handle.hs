{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy         #-}
#endif

{-# Liberally borrowed from Data.ByteString #-}

module Nullpipe.Handle
    ( hGetTillNull
    ) where

import           Control.Exception        (IOException, handleJust)
import           Control.Monad
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Maybe
import           Debug.Trace
import           Foreign                  hiding (void)
import           GHC.Base
import           GHC.IO.Buffer
import qualified GHC.IO.BufferedIO        as Buffered
import           GHC.IO.Exception
import           GHC.IO.Handle.Internals
import           GHC.IO.Handle.Types
import           GHC.IORef
import           GHC.Num                  (Num (..))
import           GHC.Real
import qualified Prelude                  as P
import           System.IO.Error          (isEOFError)

hGetTillNull :: Handle -> IO (Maybe ByteString)
hGetTillNull h =
  handleJust (\(e::IOException) ->
               void $ guard (isEOFError e))

             (const $ return Nothing) $ do
    l <- hGetTillNull' h
    return (Just l)


hGetTillNull' :: Handle -> IO ByteString
hGetTillNull' h =
  wantReadableHandle_ "Data.ByteString.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf !len xss = do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if (w == off + 1)
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else do
                 fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral 0
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw


mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 create len $ \p ->
   withRawBuffer buf $ \pbuf -> do
   copyBytes p (pbuf `plusPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)
