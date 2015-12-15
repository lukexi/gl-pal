module Graphics.GL.Pal.Utility where
import Control.Monad.Trans
import Control.Monad
import Data.Time

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show

profile :: MonadIO m => String -> Int -> m b -> m b
profile name indent action = do
  -- getCPUTime does not work on Windows >:() so we use getCurrentTime instead
  before <- liftIO getCurrentTime
  x <- action
  after <- liftIO getCurrentTime
  let diff = after `diffUTCTime` before
      tabs = replicate indent '\t'
  -- when (diff > 1/180) $ 
  when (diff > 0) $ 
    liftIO $ putStrLn (tabs ++ name ++ " Computation time: " ++ show diff)
  return x

getNow :: (Fractional a, MonadIO m) => m a
getNow = realToFrac . utctDayTime <$> liftIO getCurrentTime
