{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import Options.Applicative

import GHC.RTS.Events.SQLite (indexing, querying)

main :: IO ()
main = execParser opts >>= \case
    IndexEvents eventlog db -> indexing eventlog db
    QueryEvents db          -> querying db

-------------------------------------------------------------------------------
-- Data types

data Command
    = IndexEvents FilePath FilePath
    | QueryEvents FilePath
    deriving (Show)

-------------------------------------------------------------------------------
-- Options parser

opts :: ParserInfo Command
opts = info (cmds <**> helper) helpOpt
  where
    cmds = subparser $
           command "index" (info indexOpt (progDesc "Indexing eventlog to SQLite DB."))
        <> command "query" (info queryOpt (progDesc "Querying from the SQLite DB."))

    indexOpt :: Parser Command
    indexOpt = IndexEvents
        <$> strOption
             ( long "eventlog"
            <> short 'e'
            <> metavar "EVENTLOG"
            <> help "Location of eventlog file")
        <*> strOption
             ( long "db"
            <> short 'd'
            <> metavar "DB"
            <> help "Location of the DB file")

    queryOpt :: Parser Command
    queryOpt = QueryEvents
        <$> strOption
             ( long "db"
            <> short 'd'
            <> metavar "DB"
            <> help "Location of the DB file")

    helpOpt = fullDesc
           <> progDesc "Manipulate GHC eventlog with SQLite"
           <> header "ghc-events-sqlite"
