{-# LANGUAGE RecordWildCards, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Foreign
import Data.IORef
import Data.List
import qualified Data.Vector.Storable.Mutable as VM


data StreamingArrayBuffer a = StreamingArrayBuffer 
    { stbCapacity        :: GLuint
    , stbStreamOffsetRef :: IORef GLuint
    , stbDrawOffsetRef   :: IORef GLuint
    , stbArrayBuffer     :: ArrayBuffer
    , stbItemSize        :: GLsizei
    }

makeSAB :: forall a. Storable a => IO (StreamingArrayBuffer a)
makeSAB = do

    streamOffsetRef <- newIORef 0
    drawOffsetRef   <- newIORef 0

    arrayBuffer <- bufferDataEmpty GL_STREAM_DRAW maxInstances (Proxy :: Proxy (M44 GLfloat))

    let itemSize = sizeOf (undefined :: a)
    return StreamingArrayBuffer  
        { stbStreamOffsetRef = streamOffsetRef
        , stbDrawOffsetRef   = drawOffsetRef 
        , stbArrayBuffer     = arrayBuffer
        , stbItemSize        = fromIntegral itemSize
        , stbCapacity        = fromIntegral $ maxInstances * itemSize * 4
        }


makePowerOfTwo i = check $ find (>= i) [2 ^ x | x <- [0..]]
    where check = maybe (error $ "Broken universe error: no power of two greater than " ++ show i) id

updateSAB :: forall u a. Storable a => Shape u -> StreamingArrayBuffer a -> Int -> (Int -> IO a) -> IO ()
updateSAB shape StreamingArrayBuffer{..} numInstances getItemForIndex = do
    -- stream variables
    
    streamOffset <- readIORef stbStreamOffsetRef
         
    -- orphan the buffer if full
    let newStreamDataSize = fromIntegral $ makePowerOfTwo $ numInstances * fromIntegral stbItemSize
    
    when (streamOffset + newStreamDataSize > stbCapacity) $ do
        -- allocate new space and reset the vao
        withArrayBuffer stbArrayBuffer $
            glBufferData GL_ARRAY_BUFFER
                         (fromIntegral stbCapacity)
                         nullPtr
                         GL_STREAM_DRAW
        withShape shape $ do
            shader <- asks sProgram
            withArrayBuffer stbArrayBuffer $ 
                assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

        -- reset offset
        writeIORef stbStreamOffsetRef 0
    
    -- get memory safely
    withArrayBuffer stbArrayBuffer $ do
        bufferPtr <- castPtr <$> glMapBufferRange GL_ARRAY_BUFFER 
                                                  (fromIntegral streamOffset) 
                                                  (fromIntegral newStreamDataSize)
                                                  (GL_MAP_WRITE_BIT .|. GL_MAP_UNSYNCHRONIZED_BIT)
        let _ = bufferPtr :: Ptr a

        -- make sure memory is mapped
        when (bufferPtr == nullPtr) $
            error "Failed to map buffer."
        
        -- set final data
        loopM numInstances $ \i -> do 
            item <- getItemForIndex i
            -- (equivalent to: poke (addr `plusPtr` (idx * sizeOf x)) x)
            pokeElemOff bufferPtr i item

        -- unmap buffer
        glUnmapBuffer GL_ARRAY_BUFFER
     
    -- compute draw offset
    writeIORef stbDrawOffsetRef (streamOffset `div` fromIntegral stbItemSize)
    
    -- increment offset
    modifyIORef' stbStreamOffsetRef (+ newStreamDataSize)

drawSAB shape StreamingArrayBuffer{..} numInstances = do
    -- draw
    drawOffset   <- readIORef stbDrawOffsetRef
    withShape shape $ do
        drawShapeInstancedBaseInstance numInstances drawOffset



data Uniforms = Uniforms 
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

maxInstances :: Num a => a
maxInstances = 10000

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
  
    transformsVector   <- VM.replicate maxInstances (identity :: M44 GLfloat)
    colorsVector       <- VM.replicate maxInstances (V4 0 0 0 0 :: V4 GLfloat)
    transformsBuffer   <- bufferDataV GL_DYNAMIC_DRAW transformsVector
    colorsBuffer       <- bufferDataV GL_DYNAMIC_DRAW colorsVector
    withShape cubeShape $ do
        withArrayBuffer transformsBuffer $ 
            assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT
        withArrayBuffer colorsBuffer $ 
            assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    
    whileWindow win $ do
        processEvents events $ closeOnEscape win

        projection <- getWindowProjection win 45 0.1 1000
        (x,y,w,h)  <- getWindowViewport win
        glViewport x y w h
        
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        
        let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)
    
        t <- (*10) . realToFrac . utctDayTime <$> getCurrentTime

        profileMS ("writeVectors") 0 $ do 
            loopM maxInstances (\i -> VM.write transformsVector i (generateTransforms t i))
            loopM maxInstances (\i -> VM.write colorsVector i (generateColors t i))

        bufferSubDataV transformsBuffer transformsVector
        bufferSubDataV colorsBuffer    colorsVector
        
        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawShapeInstanced maxInstances
            
        swapBuffers win

