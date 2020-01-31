{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Hasql.NotificationSpec
  ( spec
  ) where

import           Control.Exception
import           Data.Either
import           Data.Maybe
import qualified Database.PostgreSQL.LibPQ as PQ
import           Hasql.Connection
import           Hasql.Notification
import           Hasql.Session
import           Test.Hspec
import           Database.Postgres.Temp as Temp
import           Control.Concurrent.Async
import           Data.IORef
import           Control.Concurrent
import           Data.Foldable
import           Control.Monad ((<=<))
import           Data.ByteString (ByteString)
import           Database.PostgreSQL.Simple.Options as Options

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do

        thread <- async $ do
          withFunc $ \x -> do
            putMVar var x
            takeMVar stopper
          pure $ error "Don't evaluate this"

        writeIORef asyncer $ Just thread

        either pure pure =<< (wait thread `race` takeMVar var)

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ cancel =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith

withDBConn :: Options -> ((Connection, Connection) -> IO a) -> IO a
withDBConn opts f =
  bracket (either (throwIO . userError . show) pure =<< acquire (Options.toConnectionString opts))
          release
     $ \conn -> bracket (either (throwIO . userError . show) pure =<< acquire (Options.toConnectionString opts))
                release
                $ \conn1 -> f (conn, conn1)

withSetup :: ((Connection, Connection) -> IO ()) -> IO ()
withSetup f = either throwIO pure <=< withDbCache $ \dbCache ->
  withConfig (verboseConfig <> cacheConfig dbCache) $ \db -> do
    let localhostOpts = (toConnectionOptions db)
          { host = pure "localhost"
          }

    withDBConn localhostOpts f

getNotNonBlock :: Connection -> IO (Maybe Notification)
getNotNonBlock = getNotificationNonBlocking' withConn

withConn :: Connection -> (PQ.Connection -> IO a) -> IO a
withConn = withLibPQConnection

execute :: Connection -> ByteString -> IO (Either QueryError ())
execute conn t = run (sql t) conn

spec :: Spec
spec = aroundAll withSetup $ do
{-
  describe "getNotificationNonBlocking" $ do
    it "should not return a notification" $ \conn -> do
      _ <- execute conn "LISTEN channel"
      notification <- getNotNonBlock conn
      notification `shouldSatisfy` isNothing
-}

  describe "getNotification'" $ it "should return a notification" $ \(conn, conn1) -> do
    -- The lock ensures that getNotification can’t block the connection while it is waiting
    -- lock <- newEmptyMVar
    _ <- execute conn1 "LISTEN channel"
    _ <-
     forkIO $
     do putStrLn "waiting for lock"
        -- takeMVar lock
        putStrLn "got lock"
        threadDelay 1000000
        --res <- withConn conn (\c -> exec c "NOTIFY channel")
        res <- execute conn "NOTIFY channel"
        print res
        putStrLn "notified"
        pure ()
--    notification <-
--     getNotification' (\c f -> withConn c (test lock f))
--                      conn
    Right _ <- getNotification conn1
    -- notification `shouldSatisfy` isRight
    pure ()

{-
test :: MVar () -> (t -> IO b) -> t -> IO b
test lock f c = do putStrLn "filling lock"
                   result <- tryPutMVar lock ()
                   putStrLn $  "put result: " ++ show result
                   x <- f c
                   putStrLn "performed op on libpq conn"

                   pure x
-}
{-
-- For some reason it seems to be important that the notify message is
-- send using this function rather than simply combining sendQuery w
-- ith getResult like hasql does. Otherwise random race conditions
-- occur in which threadWaitRead deadlocks.
exec :: PQ.Connection -> ByteString -> IO (Maybe PQ.Result)
exec h theSql =
  do success <- PQ.sendQuery h theSql
     if success
        then awaitResult h
        else error "PQsendQuery failed"
  where awaitResult _h =
          do mfd <- PQ.socket h
             case mfd of
               Nothing -> error "Database.PostgreSQL.Simple.Internal.exec"
               Just fd ->
                 do -- Disabling threadWaitRead causes the bug
                    threadWaitRead fd
                    PQ.getResult h
-}
