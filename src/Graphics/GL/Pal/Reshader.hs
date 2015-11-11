{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.GL.Pal.Reshader where
import System.FSNotify
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Graphics.GL
import System.FilePath

import Graphics.GL.Pal.Shader
import Graphics.GL.Pal.Types

fileNameModified fileNames event = case event of
    Modified path _ -> takeFileName path `elem` fileNames
    _               -> False

createReshaderProgram :: String -> String -> IO (IO Program)
createReshaderProgram vsPath fsPath = withReshaderProgram vsPath fsPath return

withReshaderProgram :: String -> String -> (Program -> IO a) -> IO (IO a)
withReshaderProgram vsPath fsPath action = do
    
    eventChan <- newTChanIO

    let predicate = fileNameModified (takeFileName <$> [vsPath, fsPath])

    initialShader <- createShaderProgram vsPath fsPath
    shaderRef     <- newIORef =<< action initialShader

    _ <- forkIO $ 
        withManager $ \manager -> do
            -- start a watching job (in the background)
            _ <- watchTree manager "." predicate (atomically . writeTChan eventChan)
            forever $ threadDelay 10000000

    let checkForEvent = atomically (tryReadTChan eventChan) >>= \case
            Nothing -> readIORef shaderRef
            Just _event -> do
                newShader@(Program prog) <- createShaderProgram vsPath fsPath
                linked <- overPtr (glGetProgramiv prog GL_LINK_STATUS)
                when (linked == GL_TRUE) $ 
                    writeIORef shaderRef =<< action newShader
                readIORef shaderRef
    return checkForEvent
