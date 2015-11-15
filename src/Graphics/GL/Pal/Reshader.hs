{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.GL.Pal.Reshader where
import System.FSNotify
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.FilePath

fileNameModified :: [FilePath] -> Event -> Bool
fileNameModified fileNames event = case event of
    Modified path _ -> takeFileName path `elem` fileNames
    _               -> False

watchFiles :: [FilePath] -> IO (IO (Maybe Event))
watchFiles filePaths = do
    
    eventChan <- newTChanIO

    let predicate = fileNameModified (takeFileName <$> filePaths)

    _ <- forkIO $ 
        withManager $ \manager -> do
            -- start a watching job (in the background)
            _ <- watchTree manager "." predicate (atomically . writeTChan eventChan)
            forever $ threadDelay 10000000

    let checkForEvent = atomically (tryReadTChan eventChan)
    return checkForEvent
