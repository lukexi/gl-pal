module Graphics.GL.Pal.Utility where
import Control.Monad.Trans
import Control.Monad
import Data.Time
import Debug.Trace
import Foreign

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show

profileInUnits :: MonadIO m => (Float -> Float) -> String -> String -> Int -> m b -> m b
profileInUnits toUnit unitName name indent action = do
    -- getCPUTime does not work on Windows >:() so we use getCurrentTime instead
    before <- liftIO getCurrentTime
    x <- action
    after <- liftIO getCurrentTime
    let diffS = realToFrac (after `diffUTCTime` before) :: Float
        tabs  = replicate indent '\t'
    -- when (diff > 1/180) $ 
    when (diffS > 0) $ do
      let time  = toUnit diffS
          unit  = unitName
      liftIO $ putStrLn (tabs ++ name ++ " Computation time: " ++ show time ++ unit)
    return x

profileFPS :: MonadIO m => String -> Int -> m b -> m b
profileFPS = profileInUnits (1/) "fps"

profileMS :: MonadIO m => String -> Int -> m b -> m b
profileMS  = profileInUnits (*1000) "ms"
profileS :: MonadIO m => String -> Int -> m b -> m b
profileS   = profileInUnits id "s"

profile :: MonadIO m => String -> Int -> m b -> m b
profile = profileMS 

getNow :: (Fractional a, MonadIO m) => m a
getNow = realToFrac . utctDayTime <$> liftIO getCurrentTime

traceL :: Show a => String -> a -> a
traceL label value = trace (label ++ ": " ++ show value) value

-- | Utility for extracting a value from a pointer-taking function
overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))



fI :: (Integral a , Num b) => a -> b
fI = fromIntegral
