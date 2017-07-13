{-# LANGUAGE OverloadedStrings #-}
module JobLeadsDiary.Database (

 ) where

import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple

openDatabase :: String -> IO (Connection)
openDatabase n = do
  conn <- open n
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \source(source_id BLOB PRIMARY KEY, \
    \source_name TEXT, \
    \source_add_timestamp INTEGER, \
    \source_is_blacklisted NUMERIC)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \action(action_id BLOB PRIMARY KEY, \
    \action_timestamp INTEGER, \
    \action_follow_from BLOB, \
    \action_direction INTEGER, \
    \description TEXT, \
    \FOREIGN KEY (action_follow_from) REFERENCES action(action_id) \
    \ON DELETE SET NULL"
 execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact(contact_id BLOB PRIMARY KEY, \
    \source_id BLOB, \
    \contact_name TEXT, \
    \FOREIGN KEY (source_id) REFERENCES source(source_id) ON DELETE SET NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact_detail(contact_id BLOB, \
    \contact_detail_type TEXT, \
    \contact_detail TEXT,\ 
    \PRIMARY KEY (contact_id, contact_detail_type), \
    \FOREIGN KEY (contact_id) REFERENCES contact(contact_id) ON DELETE CASCADE)"
  -- Linking tables
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \action_to_source(action_id BLOB, source_id BLOB, \
    \PRIMARY KEY (action_id, source_id), \
    \FOREIGN KEY (action_id) REFERENCES action(action_id) ON DELETE CASCADE, \
    \FOREIGN KEY (source_id) REFERENCES source(source_id) ON DELETE CASCADE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact_to_action(action_id BLOB, contact_id BLOB, \
    \PRIMARY KEY (action_id,contact_id), \
    \FOREIGN KEY (action_id) REFERENCES action(action_id) ON DELETE CASCADE, \
    \FOREIGN KEY (contact_id) REFERENCES contact(contact_id) ON DELETE CASCADE)"
 return conn
