{-# LANGUAGE OverloadedStrings,RankNTypes #-}
module JobLeadsDiary.Database (
  DBM,
  withDatabase,
  float,
  forkDBM,
  refDB,
  unrefDB
 ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import GHC.Stack.Types(HasCallStack(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple

newtype DBM a = DBM (TVar Integer -> Connection -> IO a)

instance Functor DBM where
  fmap f (DBM a) = DBM ((fmap f .) . a)

instance Applicative DBM where
  pure = DBM . const . const .  pure
  DBM f <*> DBM a = DBM (\t c -> f t c <*> a t c)

instance Monad DBM where
  return = pure
  DBM a >>= f = DBM (\t c -> a t c >>= \a' -> let
    DBM b = f a'
    in b t c
   )
  fail = DBM . const . fail

instance MonadIO DBM where
  liftIO = DBM . const . const

withDatabase :: String -> DBM a -> IO a
withDatabase fn (DBM a) = do
  t <- newTVarIO 0
  c <- openDatabase fn
  a t c <* do
    atomically $ do
      tc <- readTVar t
      when (tc > 0) retry
    close c

getConnection :: DBM Connection
getConnection = DBM (\_ t -> return t)

float :: DBM a -> DBM (IO a)
float (DBM a) = DBM (\t c -> return $ a t c)

forkDBM :: DBM () -> DBM ThreadId
forkDBM (DBM a) = DBM (\t c -> do
  atomically $ modifyTVar t (+ 1)
  forkIO $ a t c *> atomically (modifyTVar t (subtract 1))
 )

refDB :: DBM ()
refDB = DBM (\t _ -> atomically $ modifyTVar t (+ 1))

unrefDB :: DBM ()
unrefDB = DBM (\t _ -> atomically $ modifyTVar t (subtract 1))

parseUUID :: forall r . forall a . HasCallStack => BL.ByteString -> UUID.UUID
parseUUID bs = case UUID.fromByteString bs of
  Nothing -> error $ "ByteString is wrong length to represent UUID"
  Just u -> u

newtype Source = Source UUID.UUID

getSource :: T.Text -> DBM Source
getSource n = DBM (\_ c -> do
  es <- query c "SELECT source_id FROM source WHERE source_name = ?" (Only n)
  case es of
    [Only bs] -> return (Source $ parseUUID bs)
    [] -> do
      nid <- UUID.nextRandom
      ts <- getCurrentTime
      execute c "INSERT INTO source(source_id, source_name, \
        \source_add_timestamp, source_is_blacklisted) VALUES (?,?,?,0)"
        (UUID.toByteString nid,n,ts)
      return (Source nid)
    _ -> error "Multiple sources with same name in database"
 )

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
    \ON DELETE SET NULL)"
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
