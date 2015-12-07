module Graphics.GL.Pal.Utility where
import Control.Monad.Trans
import Control.Monad
import Data.Time

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show


-- profile :: MonadIO m => String -> Int -> m b -> m b
-- profile name indent action = do
--   before <- liftIO getCPUTime -- DOESN'T WORK ON WINDOWS
--   x <- action
--   after <- liftIO getCPUTime
--   let diff = (fromIntegral (after - before)) / (10^(12::Integer))
--       tabs = replicate indent '\t'
--   when (diff > 0.000000) $ 
--     liftIO $ printf "%s%s Computation time: %0.5f sec\n" tabs name (diff :: Double)
--   return x

profile :: MonadIO m => String -> Int -> m b -> m b
profile name indent action = do
  before <- liftIO getCurrentTime
  x <- action
  after <- liftIO getCurrentTime
  let diff = after `diffUTCTime` before
      tabs = replicate indent '\t'
  when (diff > 1/180) $ 
    liftIO $ putStrLn (tabs ++ name ++ " Computation time: " ++ show diff)
  return x