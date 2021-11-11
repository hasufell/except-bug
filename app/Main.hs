{-# LANGUAGE DataKinds #-}

module Main where


import Control.Monad
import Control.Monad.IO.Class
import Haskus.Utils.Variant.Excepts


data MyError = MyError String


main :: IO ()
main = do
  void $ runE ex1
  void $ runE ex2
  
 where
  ex1 :: MonadIO m => Excepts '[MyError] m ()
  ex1 = void $ forM [1, 2] $ \i -> do
              liftIO $ putStrLn $ show i
              throwE (MyError $ show i)

  ex2 :: MonadIO m => Excepts '[MyError] m ()
  ex2 = forM_ [1, 2] $ \i -> do
              liftIO $ putStrLn $ show i
              throwE (MyError $ show i)
