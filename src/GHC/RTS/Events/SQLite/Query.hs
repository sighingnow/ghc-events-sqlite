{-# LANGUAGE OverloadedStrings #-}

module GHC.RTS.Events.SQLite.Query
    ( querying
    ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import GHC.RTS.Events

import qualified Database.SQLite.Simple as DB

-------------------------------------------------------------------------------
-- Query events

querying :: FilePath -> IO ()
querying db = do
    conn <- DB.open db
    t1 <- getCurrentTime
    rs <- DB.query_ conn "SELECT * FROM evts where timestamp >= 4944846068 and timestamp <= 67837968540 ORDER BY timestamp ASC;" :: IO [(Int, Int, Maybe Int, ByteString)]
    t2 <- getCurrentTime
    print (length rs)
    print (realToFrac (diffUTCTime t2 t1))
    DB.close conn