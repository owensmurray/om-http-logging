{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Description: Logging utilities for WAI http servers. -}
module OM.HTTP.Logging (
  logExceptionsAndContinue,
  requestLogging,
) where


import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, throwM, tryAny)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger.CallStack (Loc, LogLevel, LogSource, LogStr,
  MonadLoggerIO, logError, logInfo, runLoggingT)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Network.HTTP.Types (Status(statusCode, statusMessage),
  internalServerError500)
import Network.Wai (Request(rawPathInfo, rawQueryString, requestMethod),
  Middleware, Response, ResponseReceived, responseLBS, responseStatus)


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
      logError
        $ "Internal Server Error [" <> showt uuid <> "]: "
        <> showt err
      return uuid


{- | Like 'show', but for any string-like thing. -}
showt :: (Show a, IsString b) => a -> b
showt = fromString . show


{- |
  Logs an HTTP request by emitting two log messages. The first messages
  logs that the request has begun. The second messages logs the status
  result and timing of the request once it is finished.

  > Starting request: GET /foo
  > GET /foo --> 200 Ok (0.001s)

  This can help debugging requests that hang or crash for whatever reason.
-}
requestLogging
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Middleware
requestLogging logging app req respond =
    (`runLoggingT` logging) $ do
      logInfo $ "Starting request: " <> reqStr
      liftIO . app req . loggingRespond =<< liftIO getCurrentTime
  where
    {- | Delegate to the underlying responder, and do some logging. -}
    loggingRespond :: UTCTime -> Response -> IO ResponseReceived
    loggingRespond start response = (`runLoggingT` logging) $ do
      {-
        Execute the underlying responder first so we get an accurate
        measurement of the request duration.
      -}
      ack <- liftIO $ respond response
      now <- liftIO getCurrentTime
      logInfo
        $ reqStr <> " --> " <> showStatus (responseStatus response)
        <> " (" <> showt (diffUTCTime now start) <> ")"
      return ack

    {- | A Text representation of the request, suitable for logging. -}
    reqStr :: Text
    reqStr = decodeUtf8
      $ requestMethod req <> " " <> rawPathInfo req <> rawQueryString req

    {- |
      @instance Show Status@ shows the Haskell structure, which is
      not suitable for logging.
    -}
    showStatus :: Status -> Text
    showStatus stat =
      (showt . statusCode) stat <> " " <> (decodeUtf8 . statusMessage) stat


