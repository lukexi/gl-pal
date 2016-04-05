{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Foreign
import Data.IORef

data Uniforms = Uniforms 
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

maxInstances :: Num a => a
maxInstances = 10000

streamingBufferCapacity = maxInstances * 16

generateTransforms :: Integral a => GLfloat -> a -> M44 GLfloat
generateTransforms t i = 
    let x = fromIntegral $ (i `div` 100) - 50  :: GLfloat
        y = fromIntegral $ (i `mod` 100) - 50 :: GLfloat
        m44 = mkTransformation 
                (axisAngle (V3 1 1 0) 1) 
                (V3 x (y + sin (t + fromIntegral i)) 0)
    in m44

generateColors :: Integral a => GLfloat -> a -> V4 GLfloat
generateColors t i = hslColor hue 0.9 0.6
    where hue = fromIntegral i / maxInstances + sin t

loopM :: (Monad m) => Int -> (Int -> m ()) -> m ()
loopM n action = go 0
    where
        go !i
            | i == (n-1) = action i
            | otherwise  = action i >> go (i + 1)

main :: IO ()
main = do
    (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768
  
    shader        <- createShaderProgram "test/geoInstancedM44.vert" "test/geoInstancedM44.frag"
  
    cubeGeo       <- cubeGeometry 0.5 1
    cubeShape     <- makeShape cubeGeo shader
    

    sab <- makeSAB
    transformsBuffer <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (M44 GLfloat))
    colorsBuffer     <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (V4  GLfloat))
    
    let resetShapeInstanceBuffers = withShape cubeShape $ do

            withArrayBuffer transformsBuffer $ do
                resetSABBuffer sab transformsBuffer
                assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

            withArrayBuffer colorsBuffer $ do
                resetSABBuffer sab colorsBuffer
                assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4

    
    resetShapeInstanceBuffers
        
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    
    whileWindow win $ do
        processEvents events $ closeOnEscape win

        projection <- getWindowProjection win 45 0.1 1000
        (x,y,w,h)  <- getWindowViewport win
        glViewport x y w h
        
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        
        let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)
    
        t <- (*3) . realToFrac . utctDayTime <$> getCurrentTime

        checkSAB sab maxInstances resetShapeInstanceBuffers
        fillSABBuffer sab transformsBuffer maxInstances (return . generateTransforms t)
        fillSABBuffer sab colorsBuffer     maxInstances (return . generateColors t)
        updateSAB sab maxInstances
        
        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawSAB sab maxInstances
            
        swapBuffers win



data StreamingArrayBuffer = StreamingArrayBuffer 
    { stbCapacity        :: GLuint
    , stbStreamOffsetRef :: IORef GLuint
    , stbDrawOffsetRef   :: IORef GLuint
    }

makeSAB :: IO StreamingArrayBuffer
makeSAB = do

    streamOffsetRef <- newIORef 0
    drawOffsetRef   <- newIORef 0

    return StreamingArrayBuffer  
        { stbStreamOffsetRef = streamOffsetRef
        , stbDrawOffsetRef   = drawOffsetRef 
        , stbCapacity        = fromIntegral streamingBufferCapacity
        }

checkSAB sab@StreamingArrayBuffer{..} numNewItems resetAction = do
    streamOffset <- liftIO $ readIORef stbStreamOffsetRef
         
    -- orphan the buffer if full
    
    when (streamOffset + numNewItems > stbCapacity) $ do
        resetAction

        -- reset offset
        liftIO $ writeIORef stbStreamOffsetRef 0

-- Must be called with an ArrayBuffer bound (e.g. withArrayBuffer)
resetSABBuffer :: forall a m. (MonadIO m, Storable a) => StreamingArrayBuffer -> ArrayBuffer a -> m ()
resetSABBuffer StreamingArrayBuffer{..} _ = 
    glBufferData GL_ARRAY_BUFFER
        (fromIntegral stbCapacity * fromIntegral (sizeOf (undefined :: a)))
        nullPtr
        GL_STREAM_DRAW

fillSABBuffer :: forall a. Storable a => StreamingArrayBuffer -> ArrayBuffer a -> GLuint-> (Int -> IO a) -> IO ()
fillSABBuffer StreamingArrayBuffer{..} arrayBuffer numInstances getItemForIndex = do
    streamOffset <- readIORef stbStreamOffsetRef
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
            pokeElemOff bufferPtr i item

        -- unmap buffer
        _ <- glUnmapBuffer GL_ARRAY_BUFFER
        return ()

updateSAB :: StreamingArrayBuffer -> GLuint -> IO ()
updateSAB  StreamingArrayBuffer{..} numInstances = do
    
    -- compute draw offset
    streamOffset <- readIORef stbStreamOffsetRef

    writeIORef stbDrawOffsetRef streamOffset
    
    -- increment offset
    modifyIORef' stbStreamOffsetRef (+ numInstances)

drawSAB StreamingArrayBuffer{..} numInstances = do
    -- draw
    drawOffset <- liftIO $ readIORef stbDrawOffsetRef
    drawShapeInstancedBaseInstance numInstances drawOffset