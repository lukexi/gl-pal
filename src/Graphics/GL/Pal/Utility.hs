module Graphics.GL.Pal.Utility where
import Control.Monad.Trans
import Control.Monad
import System.Clock
import Data.Time
import Debug.Trace
import Foreign

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

printIO :: (Show s, MonadIO m) => s -> m ()
printIO = putStrLnIO . show

nsToMS = ((/ 1000000) . fromIntegral)
nsToS = ((/ 1000) . nsToMS)
nsToFPS = ((1/) . nsToS)

profileInUnits :: MonadIO m => (Integer -> Float) -> String -> String -> Int -> m b -> m b
profileInUnits toUnit unitName name indent action = do
    let clockType = Monotonic
    --let clockType = Realtime
    before <- liftIO (getTime clockType)
    x <- action
    after <- liftIO (getTime clockType)
    let nanoSecDiff = timeSpecAsNanoSecs $ after `diffTimeSpec` before
        tabs  = replicate indent '\t'
        marker = if indent == 0 then "╚ " else "╠ "
    -- when (nanoSecDiff > 0) $ do
    do
        let time  = toUnit nanoSecDiff
            unit  = unitName
        liftIO $ putStrLn (marker ++ tabs ++ name ++ " : " ++ show time ++ unit)
    return x

profileFPS :: MonadIO m => String -> Int -> m b -> m b
profileFPS = profileInUnits nsToFPS "fps"

profileMS :: MonadIO m => String -> Int -> m b -> m b
profileMS  = profileInUnits nsToMS "ms"

profileS :: MonadIO m => String -> Int -> m b -> m b
profileS   = profileInUnits nsToS "s"

profile :: MonadIO m => String -> Int -> m b -> m b
profile = profileMS 

getNow :: (Fractional a, MonadIO m) => m a
getNow = realToFrac . utctDayTime <$> liftIO getCurrentTime

traceL :: Show a => String -> a -> a
traceL label value = trace (label ++ ": " ++ show value) value

-- | Utility for extracting a value from a pointer-taking function
overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))


fI :: (Integral a, Num b) => a -> b
fI = fromIntegral
