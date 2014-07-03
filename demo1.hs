{-# LANGUAGE DeriveDataTypeable #-}
module Main
  where

import Control.Monad
import Data.Dynamic

data Height = Height Int deriving (Typeable)
data Weight = Weight Double deriving (Typeable)
data Name = Name String deriving (Typeable)

props :: [Dynamic]
props = [toDyn (Name "who I am"), toDyn (Height 170), toDyn (Weight 45.1)]

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust target act = case target of
  Nothing -> return ()
  Just a -> act a

main = do
  forM_ props $ \prop -> do
    whenJust (fromDynamic prop) (\(Height h) -> putStrLn $ "height " ++ show h ++ " cm")
    whenJust (fromDynamic prop) (\(Weight w) -> putStrLn $ "weight " ++ show w ++ " kg")
    whenJust (fromDynamic prop) (\(Name n) -> putStrLn $ "I am " ++ n)
