module Utils where

import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Client

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient clientEnv x = runClientM x clientEnv

-- | Invoke an action until getting 'Just'.
--
-- The first argument describes attempted delays prior to running an action,
-- in mcs.
pollWith :: HasCallStack => [Int] -> IO (Maybe a) -> IO a
pollWith allDelays action = withFrozenCallStack $ go allDelays
  where
    go [] =
      error $
        "poll: failed to get an expected entry after "
          <> show (fromIntegral (sum allDelays) / 1e6 :: Float)
          <> " seconds"
    go (delay : remDelays) = do
      threadDelay delay
      action >>= \case
        Just x -> return x
        Nothing -> go remDelays

expBackoff :: Int -> Int -> [Int]
expBackoff startDelay maxDelay =
  0 : takeWhile (< maxDelay) (iterate (* 2) startDelay)

-- | 'pollWith' with default timing.
--
-- Optimized for requesting a server for a result of async action.
poll :: HasCallStack => IO (Maybe a) -> IO a
poll = pollWith (expBackoff 0.1e6 10e6)

getLoggerCfg :: String -> T.LoggerConfig
getLoggerCfg t =
  T.defaultLoggerConfig
    { T._logToFile = True,
      T._logFilePath = "/tmp/log-" <> t,
      T._isAsync = False
    }
