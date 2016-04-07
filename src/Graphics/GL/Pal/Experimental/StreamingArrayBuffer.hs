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

-- FIXME: should have makeSAB take a maxInstanceCount and a multiplier to make it clear that
-- it is critical to pass a decently sized multiplier of maxInstanceCount to get the most
-- benefit; otherwise you'll be reallocating buffers on every render.
-- Have makeSAB work like this for creating the buffers:
-- (transforms, colors) <- makeSAB maxInstanceCount 16 $ \addSABBuffer -> do
--      transforms <- addSABBuffer (Proxy :: Proxy (M44 GLfloat))
--      colors     <- addSABBuffer (Proxy :: Proxy (M44 GLfloat))
--      return (transforms, colors)
-- where addSABBuffer is a partially-applied call to
-- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity
-- so that the buffers are all sized correctly!

-- the call to resetShapeInstanceBuffers can probably be made friendlier too, automatically calling it once.
makeSAB :: (MonadIO m, Integral a) => a -> m StreamingArrayBuffer
makeSAB streamingBufferCapacity = liftIO $ do

    streamOffsetRef <- newIORef 0
    drawOffsetRef   <- newIORef 0

    return StreamingArrayBuffer  
        { stbStreamOffsetRef = streamOffsetRef
        , stbDrawOffsetRef   = drawOffsetRef 
        , stbCapacity        = fromIntegral streamingBufferCapacity
        }

whenSABReset :: MonadIO m => StreamingArrayBuffer -> GLuint -> IO () -> m ()
whenSABReset StreamingArrayBuffer{..} numNewItems resetAction = liftIO $ do
    streamOffset <- readIORef stbStreamOffsetRef
         
    -- orphan the buffer if full
    
    when (streamOffset + numNewItems > stbCapacity) $ do
        _ <- resetAction

        -- reset offset
        writeIORef stbStreamOffsetRef 0

-- Must be called with an ArrayBuffer bound (e.g. withArrayBuffer)
resetSABBuffer :: forall a m. (MonadIO m, Storable a) => StreamingArrayBuffer -> ArrayBuffer a -> m ()
resetSABBuffer StreamingArrayBuffer{..} _ = 
    glBufferData GL_ARRAY_BUFFER
        (fromIntegral stbCapacity * fromIntegral (sizeOf (undefined :: a)))
        nullPtr
        GL_STREAM_DRAW

-- Call from within writeSAB block
fillSABBuffer :: forall a m. (MonadIO m, Storable a, MonadReader (StreamingArrayBuffer, GLuint) m) 
              => ArrayBuffer a -> (Int -> m a) -> m ()
fillSABBuffer arrayBuffer getItemForIndex = do
    (StreamingArrayBuffer{..}, numInstances) <- ask
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

writeSAB :: MonadIO m 
         => StreamingArrayBuffer 
         -> GLuint 
         -> IO ()
         -> ReaderT (StreamingArrayBuffer, GLuint) m a1
         -> m ()
writeSAB sab numInstances resetInstanceBuffersAction fillActions = do
    whenSABReset sab numInstances resetInstanceBuffersAction
    _ <- runReaderT fillActions (sab, numInstances)
    updateSABOffsets sab numInstances