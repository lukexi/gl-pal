{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}


module Graphics.GL.Pal.Experimental.StreamingArrayBuffer where

import Graphics.GL
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Shape
import Graphics.GL.Pal.WithActions

import Foreign
import Control.Monad.Reader
import Data.IORef

loopM :: (Monad m) => Int -> (Int -> m ()) -> m ()
loopM n action = go 0
    where
        go !i
            | i == (n-1) = action i
            | otherwise  = action i >> go (i + 1)

data StreamingArrayBuffer = StreamingArrayBuffer 
    { stbCapacity        :: GLuint
    , stbStreamOffsetRef :: IORef GLuint
    , stbDrawOffsetRef   :: IORef GLuint
    }

makeSAB :: (MonadIO m, Integral a) => a -> m StreamingArrayBuffer
makeSAB streamingBufferCapacity = liftIO $ do

    streamOffsetRef <- newIORef 0
    drawOffsetRef   <- newIORef 0

    return StreamingArrayBuffer  
        { stbStreamOffsetRef = streamOffsetRef
        , stbDrawOffsetRef   = drawOffsetRef 
        , stbCapacity        = fromIntegral streamingBufferCapacity
        }

whenSABReset :: MonadIO m => StreamingArrayBuffer -> GLuint -> m a -> m ()
whenSABReset StreamingArrayBuffer{..} numNewItems resetAction = do
    streamOffset <- liftIO $ readIORef stbStreamOffsetRef
         
    -- orphan the buffer if full
    
    when (streamOffset + numNewItems > stbCapacity) $ do
        _ <- resetAction

        -- reset offset
        liftIO $ writeIORef stbStreamOffsetRef 0

-- Must be called with an ArrayBuffer bound (e.g. withArrayBuffer)
resetSABBuffer :: forall a m. (MonadIO m, Storable a) => StreamingArrayBuffer -> ArrayBuffer a -> m ()
resetSABBuffer StreamingArrayBuffer{..} _ = 
    glBufferData GL_ARRAY_BUFFER
        (fromIntegral stbCapacity * fromIntegral (sizeOf (undefined :: a)))
        nullPtr
        GL_STREAM_DRAW

fillSABBuffer :: forall a m. (MonadIO m, Storable a) => StreamingArrayBuffer -> GLuint-> ArrayBuffer a -> (Int -> m a) -> m ()
fillSABBuffer StreamingArrayBuffer{..} numInstances arrayBuffer  getItemForIndex = do
    streamOffset <- liftIO $ readIORef stbStreamOffsetRef
    -- get memory safely
    withArrayBuffer arrayBuffer $ do
        bufferPtr <- castPtr <$> glMapBufferRange GL_ARRAY_BUFFER 
                                                  (fromIntegral streamOffset * fromIntegral (sizeOf (undefined :: a))) 
                                                  (fromIntegral numInstances * fromIntegral (sizeOf (undefined :: a)))
                                                  (GL_MAP_WRITE_BIT .|. GL_MAP_UNSYNCHRONIZED_BIT)
        let _ = bufferPtr :: Ptr a

        -- make sure memory is mapped
        when (bufferPtr == nullPtr) $
            error "Failed to map buffer."
        
        -- set final data
        loopM (fromIntegral numInstances) $ \i -> do 
            item <- getItemForIndex i
            liftIO $ pokeElemOff bufferPtr i item

        -- unmap buffer
        _ <- glUnmapBuffer GL_ARRAY_BUFFER
        return ()

updateSABOffsets :: MonadIO m => StreamingArrayBuffer -> GLuint -> m ()
updateSABOffsets  StreamingArrayBuffer{..} numInstances = liftIO $ do
    
    -- compute draw offset
    streamOffset <- readIORef stbStreamOffsetRef

    writeIORef stbDrawOffsetRef streamOffset
    
    -- increment offset
    modifyIORef' stbStreamOffsetRef (+ numInstances)

drawSAB :: (MonadIO m, MonadReader (Shape u) m) => StreamingArrayBuffer -> GLsizei -> m ()
drawSAB StreamingArrayBuffer{..} numInstances = do
    -- draw
    drawOffset <- liftIO $ readIORef stbDrawOffsetRef
    drawShapeInstancedBaseInstance numInstances drawOffset

--writeSAB sab numInstances resetInstanceBuffersAction fillActions = do
--	whenSABReset sab numInstances resetShapeInstanceBuffers
--	runReaderT fillActions (sab, numInstances)
--	updateSABOffsets sab numInstances