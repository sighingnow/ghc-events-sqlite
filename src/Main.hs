{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Database.SQLite.Simple ()
import GHC.RTS.Events ()
import Options.Applicative

main :: IO ()
main = do
    join $ execParser (info opts idm)

opts :: Parser (IO ())
opts = subparser $
       command "index" (info (index <$> argument str idm) idm)
    <> command "query" (info (pure query) idm)

index :: String -> IO ()
index arg = putStrLn $ "index arg: " <> arg

query :: IO ()
query = putStrLn $ "query..."