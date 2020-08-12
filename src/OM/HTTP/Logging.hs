{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Description: Logging utilities for WAI http servers. -}
module OM.HTTP.Logging (
  logExceptionsAndContinue,
) where


import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, throwM,
  tryAny)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr,
  MonadLoggerIO, logError, runLoggingT)
import Data.String (fromString, IsString)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Network.HTTP.Types (internalServerError500)
import Network.Wai (Middleware, Response, ResponseReceived,
  responseLBS)

{- |
  Logs all exceptions, and returns a 500 Internal Server error. 

  This is useful because your wai framework won't always do what you
  expect when it encounters random exceptions. For instance, an exception
  thrown in IO may cause functionality of higher-level middlewares to be
  bypassed unless they know how to catch and re-throw exceptions (making
  them more complicated). This middleware explicitly will not re-throw
  exceptions, unless those exceptions were encountered after the headers
  have already been sent, e.g. when using 'Network.Wai.StreamingBody'.
  
  What it will do is generate a unique id for the exception and print
  that ID, so you can easily find it in the logs.
-}
logExceptionsAndContinue
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Logging backend. -}
  -> Middleware
logExceptionsAndContinue logging app req respond = (`runLoggingT` logging) $
    tryAny (liftIO (app req loggingRespond)) >>= \case
      Right ack -> return ack
      Left err -> do
        uuid <- logProblem err
        liftIO $ respond (errResponse uuid)

  where
    errResponse :: UUID -> Response
    errResponse uuid =
      responseLBS
        internalServerError500
        [("Content-Type", "text/plain")] 
        ("Internal Server Error. Error ID: " <> showt uuid)

    getUUID :: (MonadIO m) => m UUID
    getUUID = liftIO nextUUID >>= \case
      Nothing -> liftIO (threadDelay 1000) >> getUUID
      Just uuid -> return uuid

    loggingRespond :: Response -> IO ResponseReceived
    loggingRespond response = (`runLoggingT` logging) $
      tryAny (liftIO (respond response)) >>= \case
        Right ack -> return ack
        Left err -> do
          void $ logProblem err
          throwM err

    logProblem :: (MonadLoggerIO m) => SomeException -> m UUID
    logProblem err = do
      uuid <- getUUID
      $(logError)
        $ "Internal Server Error [" <> showt uuid <> "]: "
        <> showt err
      return uuid


{- | Like 'show', but for any string-like thing. -}
showt :: (Show a, IsString b) => a -> b
showt = fromString . show


