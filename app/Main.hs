{-# LANGUAGE DataKinds #-}

module Main where


import Control.Monad
import Control.Monad.IO.Class
import Haskus.Utils.Variant.Excepts


data MyError = MyError String
  deriving Show


main :: IO ()
main = do
  e1 <- runE ex1
  putStrLn $ show e1
  e2 <- runE ex2
  putStrLn $ show e2
  
 where
  ex1 :: MonadIO m => Excepts '[MyError] m ()
  ex1 = void $ forM [1, 2] $ \i -> do
              liftIO $ putStrLn $ show i
              throwE (MyError $ show i)

  ex2 :: MonadIO m => Excepts '[MyError] m ()
  ex2 = forM_ [1, 2] $ \i -> do
              liftIO $ putStrLn $ show i
              throwE (MyError $ show i)
