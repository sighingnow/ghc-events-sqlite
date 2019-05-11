{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.RTS.Events.SQLite.Index
    ( indexing
    ) where

import Control.Monad
import Data.Binary.Put (runPut)
import Data.Vector (Vector)
import Database.SQLite.Simple (FromRow (..), ToRow (..), Connection, Statement (..), Query (..))
import GHC.RTS.Events

import qualified Data.Vector as V
import qualified Database.SQLite3 as DB (step)
import qualified Database.SQLite.Simple as DB

-------------------------------------------------------------------------------
-- Index events

indexing :: FilePath -> FilePath -> IO ()
indexing eventlog db = readEventLogFromFile eventlog >>= \case
    Left e -> print e
    Right EventLog{..} -> toTable db (eventTypes header) (events dat)

toTable :: FilePath -> [EventType] -> [Event] -> IO ()
toTable db etypes evts = do
    conn <- DB.open db
    DB.execute_ conn "PRAGMA journal_mode = MEMORY"
    DB.execute_ conn "PRAGMA synchronous = OFF"

    -- Store event types
    DB.execute_ conn "CREATE TABLE etypes (num INTEGER PRIMARY KEY, desc TEXT, size INTEGER);"
    DB.withTransaction conn $
        DB.executeMany conn "INSERT INTO etypes (num, desc, size) VALUES (?, ?, ?)" etypes

    -- Store events
    DB.execute_ conn "CREATE TABLE evts (id INTEGER PRIMARY KEY, timestamp INTEGER, cap INTEGER, etype INTEGER, einfo TEXT);"
    DB.execute_ conn "CREATE INDEX evts_cap_timestamp ON evts (cap, timestamp);"
    DB.withTransaction conn $
        DB.executeMany conn "INSERT INTO evts (timestamp, cap, etype, einfo) VALUES (?, ?, ?, ?)" evts

    -- Done
    DB.close conn

instance ToRow EventType where
    toRow EventType{..} = toRow (num, desc, size)

instance ToRow Event where
    -- toRow Event{..} = toRow (evTime, evCap, eventTypeNum evSpec, showEventInfo evSpec)
    toRow Event{..} = toRow (evTime, evCap, eventTypeNum evSpec, runPut (putEventSpec evSpec))
