{-# LANGUAGE OverloadedStrings #-}
import           Data.IORef
import           Nullpipe
import           Nullpipe.Handle
import           Pipes
import           System.IO
import           Test.Hspec

main :: IO ()
main = hspec spec

spec = describe "nullpipe" $ do
  it "can read things with nulls in them" $ do
    h <- openFile  "./fixtures/nulled" ReadMode
    a <- hGetTillNull h
    a `shouldBe` Just "hi"
    a <- hGetTillNull h
    a `shouldBe` Just "there"
    a <- hGetTillNull h
    a `shouldBe` Nothing

  it "can do it through a nullpipe" $ do
    ref <- newIORef []
    runEffect $ for (nullpipe "./fixtures/nulled") $ \x -> liftIO $ atomicModifyIORef ref (\a -> (x:a, ()))
    res <- readIORef ref
    reverse res `shouldBe` ["hi",  "there"]
