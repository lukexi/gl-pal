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


updateSAB :: forall u a. Storable a => Shape u -> ArrayBuffer -> StreamingArrayBuffer -> GLuint -> (Int -> IO a) -> IO ()
updateSAB shape arrayBuffer StreamingArrayBuffer{..} numInstances getItemForIndex = do
    -- stream variables
    
    let numNewItems = numInstances
    do
        streamOffset <- readIORef stbStreamOffsetRef
             
        -- orphan the buffer if full
        
        when (streamOffset + numNewItems > stbCapacity) $ do
            putStrLn $ "INVALIDATING"
            -- allocate new space and reset the vao
            withArrayBuffer arrayBuffer $
                glBufferData GL_ARRAY_BUFFER
                             (fromIntegral stbCapacity * fromIntegral (sizeOf (undefined :: a)))
                             nullPtr
                             GL_STREAM_DRAW
            withShape shape $ do
                shader <- asks sProgram
                withArrayBuffer arrayBuffer $ 
                    assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

            -- reset offset
            writeIORef stbStreamOffsetRef 0
    streamOffset <- readIORef stbStreamOffsetRef
    
    -- get memory safely
    withArrayBuffer arrayBuffer $ do
        print (streamOffset, numNewItems)
        bufferPtr <- castPtr <$> glMapBufferRange GL_ARRAY_BUFFER 
                                                  (fromIntegral streamOffset * fromIntegral (sizeOf (undefined :: a))) 
                                                  (fromIntegral numNewItems * fromIntegral (sizeOf (undefined :: a)))
                                                  (GL_MAP_WRITE_BIT .|. GL_MAP_UNSYNCHRONIZED_BIT)
        let _ = bufferPtr :: Ptr a

        -- make sure memory is mapped
        when (bufferPtr == nullPtr) $
            error "Failed to map buffer."
        
        -- set final data
        loopM (fromIntegral numInstances) $ \i -> do 
            item <- getItemForIndex i
            -- (equivalent to: poke (addr `plusPtr` (idx * sizeOf x)) x)
            pokeElemOff bufferPtr i item

        -- unmap buffer
        _ <- glUnmapBuffer GL_ARRAY_BUFFER
        return ()
     
    -- compute draw offset
    writeIORef stbDrawOffsetRef streamOffset
    
    -- increment offset
    modifyIORef' stbStreamOffsetRef (+ numNewItems)

drawSAB StreamingArrayBuffer{..} numInstances = do
    -- draw
    drawOffset   <- liftIO $ readIORef stbDrawOffsetRef
    drawShapeInstancedBaseInstance numInstances drawOffset

data Uniforms = Uniforms 
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

maxInstances :: Num a => a
maxInstances = 10000

streamingBufferCapacity = maxInstances * 4

generateTransforms :: Integral a => GLfloat -> a -> M44 GLfloat
generateTransforms t i = 
    let x = fromIntegral $ (i `div` 100) - 50  :: GLfloat
        y = fromIntegral $ (i `mod` 100) - 50 :: GLfloat
        m44 = mkTransformation 
                (axisAngle (V3 1 1 0) 1) 
                (V3 x (y + sin (t + fromIntegral i)) 0)
    in m44

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
    
    let configureTransformsBuffer = withArrayBuffer transformsBuffer $ 
            assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

    withShape cubeShape $ do
        configureTransformsBuffer
        
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

        updateSAB cubeShape transformsBuffer sab maxInstances (return . generateTransforms t)  
        
        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawSAB sab maxInstances
            
        swapBuffers win

