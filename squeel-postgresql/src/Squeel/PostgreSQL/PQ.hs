{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.PQ where

import Control.Arrow
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Functor

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Value
import Squeel.PostgreSQL.Type

newtype Connection db = Connection { unConnection :: LibPQ.Connection }

newtype PQ m db0 db1 x = PQ
  { runPQ :: Connection db0 -> m (x, Connection db1) }
  deriving Functor

evalPQ :: Functor m => PQ m db0 db1 x -> Connection db0 -> m x
evalPQ (PQ pq) = fmap fst . pq

execPQ :: Functor m => PQ m db0 db1 x -> Connection db0 -> m (Connection db1)
execPQ (PQ pq) = fmap snd . pq

instance Functor m => IxFunctor (PQ m) where
  imap f (PQ g) = PQ $ fmap (first f) . g

instance Applicative m => IxPointed (PQ m) where
  ireturn x = PQ $ \ conn -> pure (x, conn)

instance Monad m => IxApplicative (PQ m) where
  iap (PQ f) (PQ x) = PQ $ \ conn -> do
    (f', conn') <- f conn
    (x', conn'') <- x conn'
    return (f' x', conn'')

instance Monad m => Applicative (PQ m db db) where
  pure = ireturn
  (<*>) = iap

instance Monad m => IxMonad (PQ m) where
  ibind f (PQ x) = PQ $ \ conn -> do
    (x', conn') <- x conn
    runPQ (f x') conn'

instance Monad m => Monad (PQ m db db) where
  return = ireturn
  (>>=) = flip ibind

instance IxMonadTrans PQ where
  ilift m = PQ $ \ conn -> do
    x <- m
    return (x, conn)

instance MonadBase b m => MonadBase b (PQ m db db) where
  liftBase = ilift . liftBase

type PQRun db = forall m x. Monad m => PQ m db db x -> m (x, Connection db)

pqliftWith :: Functor m => (PQRun db -> m a) -> PQ m db db a
pqliftWith f = PQ $ \ conn ->
  fmap (\ x -> (x, conn)) (f $ \ pq -> runPQ pq conn)

instance MonadBaseControl b m => MonadBaseControl b (PQ m db db) where
  type StM (PQ m db db) x = StM m (x, Connection db)
  liftBaseWith f =
    pqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PQ . const . restoreM

connectdb :: MonadBase IO io => ByteString -> io (Connection db)
connectdb = fmap Connection . liftBase . LibPQ.connectdb

finish :: MonadBase IO io => Connection db -> io ()
finish = liftBase . LibPQ.finish . unConnection

withConnection
  :: MonadBaseControl IO io
  => ByteString
  -> (Connection db -> io x)
  -> io x
withConnection connString action =
  bracket (connectdb connString) finish action

newtype Result xs = Result { unResult :: LibPQ.Result }

exec
  :: MonadBase IO io
  => Query db0 db1 '[] xs
  -> PQ io db0 db1 (Maybe (Result xs))
exec (Query q) = PQ $ \ (Connection conn) -> do
  result <- liftBase $ LibPQ.exec conn q
  return (Result <$> result, Connection conn)

execParams
  :: (MonadBase IO io, ToValues xs ps, ToOids ps)
  => Query db0 db1 ps ys
  -> Rec Identity xs
  -> PQ io db0 db1 (Maybe (Result ys))
execParams (Query q :: Query db0 db1 ps ys) params =
  PQ $ \ (Connection conn) -> do
    let
      params' =
        [ Just (oid, param, LibPQ.Binary)
        | (oid, param) <-
            zip (toOids (Proxy @ps)) (toValues (Proxy @ps) params)
        ]
    result <- liftBase $ LibPQ.execParams conn q params' LibPQ.Binary
    return (Result <$> result, Connection conn)

prepare
  :: (MonadBase IO io, ToOids ps)
  => ByteString
  -> Query db0 db1 ps xs
  -> PQ io db0 db0 (Maybe (Result []), PreparedQuery db0 db1 ps xs)
prepare statementName (Query q :: Query db0 db1 ps ys) =
  PQ $ \ (Connection conn) -> do
    result <- liftBase $
      LibPQ.prepare conn statementName q (Just (toOids (Proxy @ps)))
    return ((Result <$> result,PreparedQuery statementName), Connection conn)

execPrepared
  :: (MonadBase IO io, ToValues xs ps)
  => PreparedQuery db0 db1 ps ys
  -> Rec Identity xs
  -> PQ io db0 db1 (Maybe (Result ys))
execPrepared (PreparedQuery q :: PreparedQuery db0 db1 ps ys) params =
  PQ $ \ (Connection conn) -> do
    let
      params' =
        [Just (param, LibPQ.Binary) | param <- toValues (Proxy @ps) params]
    result <- liftBase $
      LibPQ.execPrepared conn q params' LibPQ.Binary
    return (Result <$> result, Connection conn)

newtype RowNumber = RowNumber { unRowNumber :: LibPQ.Row }

newtype ColumnNumber cs c = ColumnNumber { unColumnNumber :: LibPQ.Column }

colNum0 :: ColumnNumber (c0:cs) c0
colNum0 = ColumnNumber 0

colNum1 :: ColumnNumber (c0:c1:cs) c1
colNum1 = ColumnNumber 1

colNum2 :: ColumnNumber (c0:c1:c2:cs) c2
colNum2 = ColumnNumber 2

colNum3 :: ColumnNumber (c0:c1:c2:c3:cs) c3
colNum3 = ColumnNumber 3

colNum4 :: ColumnNumber (c0:c1:c2:c3:c4:cs) c4
colNum4 = ColumnNumber 4

getvalue
  :: (FromValue x y, MonadBase IO io)
  => Proxy x
  -> Result xs
  -> RowNumber
  -> ColumnNumber xs x
  -> io (Maybe (Either Text y))
getvalue proxy (Result result) (RowNumber r) (ColumnNumber c) = liftBase $
  fmap (fmap (decodeValue proxy)) (LibPQ.getvalue result r c)