{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
import           Control.Exception
import           Hasql.Connection
import           Hasql.Notification
import           Hasql.Session
import           Test.Hspec
import           Database.Postgres.Temp as Temp
import           Control.Concurrent.Async
import           Data.IORef
import           Control.Concurrent
import           Data.Foldable
import           Control.Monad ((<=<), void)
import           Data.ByteString (ByteString)
import           Database.PostgreSQL.Simple.Options as Options

main :: IO ()
main = hspec spec

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

withDBConn :: Options -> (Connection -> IO a) -> IO a
withDBConn opts f =
  bracket (either (throwIO . userError . show) pure =<< acquire (Options.toConnectionString opts))
          release
          f
withSetup :: (Connection -> IO ()) -> IO ()
withSetup f = either throwIO pure <=< withDbCache $ \dbCache ->
  withConfig (verboseConfig <> cacheConfig dbCache) $ \db -> do
    let localhostOpts = (toConnectionOptions db)
          { host = pure "localhost"
          }

    withDBConn localhostOpts f

execute :: Connection -> ByteString -> IO (Either QueryError ())
execute conn t = run (sql t) conn

spec :: Spec
spec = aroundAll withSetup $ do
  describe "getNotification'" $ it "should return a notification" $ \conn -> do
    let initialChannel = "chanel"
        initialData = "foo"
    _ <- execute conn $ "LISTEN " <> initialChannel
    _ <- forkIO $ void $ execute conn $ "NOTIFY " <> initialChannel <> ", '" <> initialData <>"'"

    Right Notification {..} <- getNotification conn
    notificationChannel `shouldBe` initialChannel
    notificationData `shouldBe` initialData
