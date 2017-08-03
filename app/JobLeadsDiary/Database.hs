{-# LANGUAGE OverloadedStrings,RankNTypes,MultiParamTypeClasses #-}
module JobLeadsDiary.Database (
  DBM,
  Source,
  Action,
  Contact,
  ActionType(..),
  withDatabase,
  forkDBM,
  getSource,
  sourcesBeginningWith,
  getSourceName,
  isSourceBlacklisted,
  newUser,
  lookupUser,
  userWithCookie,
  userSetRealName,
  userGetRealName,
  newAction,
  followAction,
  setActionTimestamp,
  getActionTimestamp,
  actionsForDateRange,
  actionsForContact,
  actionsForSource,
  newContact,
  addContactDetail,
  getContactName,
  searchContacts,
 ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Crypto.KDF.BCrypt
import GHC.Stack.Types(HasCallStack(..))
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple
import System.Mem.Weak

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

instance MonadUnliftIO DBM where
  askUnliftIO = DBM (\n c -> do
    atomically $ do
      nv <- readTVar n
      writeTVar n (nv + 1)
    let
      r = UnliftIO $ \(DBM m) -> m n c
    addFinalizer r $ atomically $ do
      nv <- readTVar n
      writeTVar n (nv - 1)
    return r
   )

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

forkDBM :: DBM () -> DBM ThreadId
forkDBM (DBM a) = DBM (\t c -> do
  atomically $ modifyTVar t (+ 1)
  forkIO $ a t c *> atomically (modifyTVar t (subtract 1))
 )

parseUUID :: forall r . forall a . HasCallStack => BL.ByteString -> UUID.UUID
parseUUID bs = case UUID.fromByteString bs of
  Nothing -> error $ "ByteString is wrong length to represent UUID"
  Just u -> u

newtype Source = Source UUID.UUID deriving (Eq,Ord)

getSource :: User -> T.Text -> DBM Source
getSource (User uid) n = DBM (\_ c -> do
  es <- query c "SELECT source_id FROM source WHERE \
    \source_name = ? AND user_id = ?" (n, UUID.toByteString uid)
  case es of
    [Only bs] -> return (Source $ parseUUID bs)
    [] -> do
      nid <- UUID.nextRandom
      ts <- getCurrentTime
      execute c "INSERT INTO source(source_id, source_name, \
        \source_add_timestamp, source_is_blacklisted, user_id) \
        \VALUES (?,?,?,0,?)"
        (UUID.toByteString nid,n,ts,UUID.toByteString uid)
      return (Source nid)
    _ -> error "Multiple sources with same name in database"
 )

sourcesBeginningWith :: User -> T.Text -> DBM [Source]
sourcesBeginningWith (User uid) n = DBM (\_ c ->
  mapMaybe (fmap Source . UUID.fromByteString . fromOnly) <$>
    query c "SELECT source_id FROM source WHERE source_name LIKE ? \
      \AND user_id = ?"
      (mappend n "%", UUID.toByteString uid)
 )

getSourceName :: Source -> DBM (Maybe T.Text)
getSourceName (Source uuid) = DBM (\_ c ->
  (fmap fromOnly . listToMaybe) <$>
  query c "SELECT source_name FROM source WHERE source_id = ?"
  (Only $ UUID.toByteString uuid)
 )

isSourceBlacklisted :: Source -> DBM Bool
isSourceBlacklisted (Source sid) = DBM (\_ c -> (any (== (Only True))) <$>
  query c "SELECT source_is_blacklisted FROM source WHERE source_id = ?"
    (Only $ UUID.toByteString sid)
 )

newtype User = User UUID.UUID deriving (Eq,Ord)

newUser :: T.Text -> DBM (Maybe User)
newUser n = DBM (\_ c -> do
  e <- query c "SELECT user_id FROM user WHERE username = ?" (Only n) ::
    IO [Only BL.ByteString]
  case e of
    [] -> do
      uid <- UUID.nextRandom
      execute c "INSERT INTO user(user_id,username) VALUES (?,?)"
        (UUID.toByteString uid, n)
      return (Just (User uid))
    _ -> return Nothing
 )

lookupUser :: T.Text -> DBM (Maybe User)
lookupUser n = DBM (\_ c -> do
  e <- query c "SELECT user_id FROM user WHERE username = ?" (Only n)
  case e of
    [Only uid] -> return (User <$> UUID.fromByteString uid)
    _ -> return Nothing
 )

userWithCookie :: T.Text -> DBM (Maybe User)
userWithCookie ct = DBM (\_ c -> do
  ts <- getCurrentTime
  l <- query c "SELECT user_id,user_cookie_expiry FROM user_cookie WHERE \
    \user_cookie_text = ?"
    (Only ct)
  case l of
    [(_,et)] | et < ts -> do
      execute c "DELETE FROM user_cookie WHERE user_cookie_text = ?" (Only ct)
      return Nothing
    [(uid,_)] -> return $ User <$> UUID.fromByteString uid
    _ -> return Nothing
 )

userSetRealName :: User -> T.Text -> DBM ()
userSetRealName (User uid) n = DBM (\_ c ->
  execute c "UPDATE user SET user_real_name = ? WHERE user_id = ?"
    (n, UUID.toByteString uid)
 )

userGetRealName :: User -> DBM T.Text
userGetRealName (User uid) = DBM (\_ c -> do
  l <- query c "SELECT user_real_name FROM user WHERE user_id = ?"
    (Only $ UUID.toByteString uid)
  case l of
    [Only t] -> return t
    _ -> fail "User not found in database"
 )

newtype Action = Action UUID.UUID deriving (Eq,Ord)
data ActionType = MyAction | Response

newAction :: User -> [Source] -> ActionType -> T.Text -> DBM Action
newAction (User uid) s y d = DBM (\_ c -> do
  ts <- getCurrentTime
  aid <- UUID.nextRandom
  execute c "INSERT INTO action(action_id,user_id,action_timestamp,\
    \action_follow_from,action_direction,description) VALUES(?,?,?,?,?,?)"
    (UUID.toByteString aid,
      UUID.toByteString uid,
      ts,
      (Nothing :: Maybe BL.ByteString),
      case y of
        MyAction -> False
        Response -> True,
      d
     )
  forM_ s $ \(Source s') -> execute c "INSERT INTO action_to_source(action_id,\
    \source_id) VALUES (?,?)" (UUID.toByteString aid, UUID.toByteString s')
  return (Action aid)
 )

followAction :: [Source] -> Action -> ActionType -> T.Text -> DBM Action
followAction s (Action ff) y d = DBM (\_ c -> do
  ts <- getCurrentTime
  aid <- UUID.nextRandom
  [Only uid] <- query c "SELECT user_id FROM action WHERE action_id = ?"
    (Only $ UUID.toByteString ff) :: IO [Only BL.ByteString]
  execute c "INSERT INTO action(action_id,user_id,action_timestamp,\
    \action_follow_from,action_direction,description) VALUES(?,?,?,?,?,?)"
    (UUID.toByteString aid,
      uid,
      ts,
      UUID.toByteString ff,
      case y of
        MyAction -> False
        Response -> True,
      d
     )
  forM_ s $ \(Source s') -> execute c "INSERT INTO action_to_source(action_id,\
    \source_id) VALUES (?,?)" (UUID.toByteString aid, UUID.toByteString s')
  return (Action aid)
 )

setActionTimestamp :: Action -> UTCTime -> DBM ()
setActionTimestamp (Action a) t = DBM (\_ c -> 
  execute c "UPDATE action SET action_timestamp = ? WHERE action_id = ?"
    (t, UUID.toByteString a)
 )

getActionTimestamp :: Action -> DBM UTCTime
getActionTimestamp (Action a) = DBM (\_ c -> do
  l <- query c "SELECT action_timestamp FROM action WHERE action_id = ?"
    (Only (UUID.toByteString a))
  case l of
    (Only r:_) -> return r
    _ -> fail "Action missing from databse"
 )

actionsForDateRange :: User -> UTCTime -> UTCTime -> DBM [Action]
actionsForDateRange (User uid) b e = DBM (\_ c -> do
  l <- query c "SELECT action_id FROM action WHERE \
    \user_id = ? AND \
    \action_timestamp >= ? AND action_timestamp <= ?"
    (UUID.toByteString uid, b, e)
  return [Action x | Only n <- l, Just x <- [UUID.fromByteString n]]
 )

actionsForContact :: Contact -> DBM [Action]
actionsForContact (Contact cid) = DBM (\_ c -> do
  l <- query c "SELECT action_id FROM contact_to_action WHERE contact_id = ?"
    (Only $ UUID.toByteString cid)
  return [Action x | Only n <- l, Just x <- [UUID.fromByteString n]]
 )

class Linkable a b where
  link :: a -> b -> DBM ()
  getLinked :: a -> DBM [b]

instance Linkable Contact Source where
  link (Contact cid) (Source sid)  = DBM (\_ c ->
    execute c "INSERT INTO contact_to_source(source_id,contact_id) VALUES(?,?)"
      (UUID.toByteString sid, UUID.toByteString cid)
   )
  getLinked (Contact cid) = DBM (\_ c -> do
    l <- query c "SELECT source_id FROM contact_to_source WHERE contact_id = ?"
      (Only $ UUID.toByteString cid)
    return [Source sid | Only x <- l, Just sid <- [UUID.fromByteString x]]
   )

instance Linkable Source Contact where
  link s c = link c s
  getLinked (Source sid) = DBM (\_ c -> do
    l <- query c "SELECT contact_id FROM contact_to_source WHERE source_id = ?"
      (Only $ UUID.toByteString sid)
    return [Contact cid | Only x <- l, Just cid <- [UUID.fromByteString x]]
   )

instance Linkable Contact Action where
  link (Contact cid) (Action aid) = DBM (\_ c ->
    execute c "INSERT INTO contact_to_action(action_id,contact_id) VALUES(?,?)"
      (UUID.toByteString aid, UUID.toByteString cid)
   )
  getLinked = actionsForContact

instance Linkable Action Contact where
  link (Action aid) (Contact cid) = DBM (\_ c ->
    execute c "INSERT INTO contact_to_action(action_id,contact_id) VALUES(?,?)"
      (UUID.toByteString aid, UUID.toByteString cid)
   )
  getLinked (Action aid) = DBM (\_ c -> do
    l <- query c "SELECT contact_id FROM contact_to_action WHERE action_id = ?"
      (Only $ UUID.toByteString aid)
    return [Contact cid | Only x <- l, Just cid <- [UUID.fromByteString x]]
   )

instance Linkable Action Source where
  link (Action aid) (Source sid) = DBM (\_ c ->
    execute c "INSERT INTO action_to_source(action_id,source_id) VALUES(?,?)"
      (UUID.toByteString aid, UUID.toByteString sid)
   )
  getLinked (Action aid) = DBM (\_ c -> do
    l <- query c "SELECT source_id FROM action_to_source WHERE action_id = ?"
      (Only $ UUID.toByteString aid)
    return $ [Source sid | Only x <- l, Just sid <- [UUID.fromByteString x]]
   )

actionsForSource :: Source -> DBM [Action]
actionsForSource (Source sid) = DBM (\_ c -> do
  l <- query c "SELECT action_id FROM action_to_source WHERE source_id = ?"
    (Only $ UUID.toByteString sid)
  return [Action x | Only n <- l, Just x <- [UUID.fromByteString n]]
 )

actionDescription :: Action -> DBM T.Text
actionDescription (Action aid) = DBM (\_ c -> do
  l <- query c "SELECT description FROM action WHERE action_id = ?"
    (Only $ UUID.toByteString aid)
  case l of
    [Only d] -> return d
    _ -> fail "Action not present in database"
 )

newtype Contact = Contact UUID.UUID deriving (Eq,Ord)

newContact :: User -> T.Text -> DBM Contact
newContact (User uid) n = DBM (\_ c -> do
  cid <- UUID.nextRandom
  execute c "INSERT INTO contact(contact_id,user_id,contact_name) \
    \VALUES (?,?,?)"
    (UUID.toByteString cid, UUID.toByteString uid, n)
  return (Contact cid)
 )

addContactDetail :: Contact -> T.Text -> T.Text -> DBM ()
addContactDetail (Contact i) t d = DBM (\_ c ->
  execute c "INSERT INTO contact_detail(contact_id,\
    \contact_detail_type,contact_detail) VALUES (?,?,?)"
    (UUID.toByteString i, t, d)
 )

getContactName :: Contact -> DBM T.Text
getContactName (Contact i) = DBM (\_ c -> do
  l <- query c "SELECT contact_name FROM contact WHERE contact_id = ?"
    (Only (UUID.toByteString i))
  case l of
    (Only n:_) -> return n
    _ -> fail "Contact ID not found in database"
 )

getContactDetails :: Contact -> DBM [(T.Text,T.Text)]
getContactDetails (Contact cid) = DBM (\_ c ->
  query c "SELECT contact_detail_type, contact_detail FROM contact_detail \
    \WHERE contact_id = ?" (Only $ UUID.toByteString cid)
 )

searchContacts :: User -> T.Text -> DBM [Contact]
searchContacts (User uid) n = DBM (\_ c -> do
  let nw = mconcat ["%",n,"%"]
  l1 <- query c "SELECT contact_id FROM contact WHERE \
    \user_id = ? AND contact_name LIKE ?"
    (UUID.toByteString uid, nw)
  l2 <- query c "SELECT contact_id FROM contact_detail \
    \WHERE user_id = ? AND contact_detail LIKE ?"
    (UUID.toByteString uid, nw)
  return $ nub [Contact cid |
    l <- [l1,l2],
    Only b <- l,
    Just cid <- [UUID.fromByteString b]
   ]
 )

openDatabase :: String -> IO (Connection)
openDatabase n = do
  conn <- open n
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \user(user_id BLOB PRIMARY KEY, \
    \username TEXT, \
    \user_pw_hash BLOB, \
    \user_real_name TEXT, \
    \user_email TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \source(source_id BLOB PRIMARY KEY, \
    \user_id BLOB, \
    \source_name TEXT, \
    \source_add_timestamp INTEGER, \
    \source_is_blacklisted NUMERIC, \
    \FOREIGN KEY (user_id) REFERENCES user(user_id) ON DELETE CASCADE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \action(action_id BLOB PRIMARY KEY, \
    \user_id BLOB, \
    \action_timestamp INTEGER, \
    \action_follow_from BLOB, \
    \action_direction INTEGER, \
    \description TEXT, \
    \FOREIGN KEY (action_follow_from) REFERENCES action(action_id) \
    \ON DELETE SET NULL, \
    \FOREIGN KEY (user_id) REFERENCES user(user_id) ON DELETE CASCADE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact(contact_id BLOB PRIMARY KEY, \
    \user_id BLOB,\
    \contact_name TEXT, \
    \FOREIGN KEY (user_id) REFERENCES user(user_id) ON DELETE CASCADE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact_detail(contact_id BLOB, \
    \contact_detail_type TEXT, \
    \contact_detail TEXT,\ 
    \PRIMARY KEY (contact_id, contact_detail_type), \
    \FOREIGN KEY (contact_id) REFERENCES contact(contact_id) ON DELETE CASCADE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \user_cookie(user_cookie_text BLOB PRIMARY KEY, \
    \user_id BLOB, \
    \user_cookie_expiry INTEGER, \
    \FOREIGN KEY (user_id) REFERENCES user(user_id) ON DELETE CASCADE)"
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
  execute_ conn "CREATE TABLE IF NOT EXISTS \
    \contact_to_source(contact_id BLOB, \
    \source_id BLOB, \
    \PRIMARY KEY (contact_id, source_id), \
    \FOREIGN KEY (contact_id) REFERENCES contact(contact_id) \
    \ON DELETE CASCADE, \
    \FOREIGN KEY (source_id) REFERENCES source(source_id) ON DELETE CASCADE)"
  return conn
