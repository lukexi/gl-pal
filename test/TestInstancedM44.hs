{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable

import System.Random

data Uniforms = Uniforms 
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

generateTransforms :: GLsizei -> GLfloat -> IO [M44 GLfloat]
generateTransforms instanceCount t = forM [0..instanceCount-1] $ \i -> do

    let x = fromIntegral $ (i `div` 100) - 50  :: GLfloat
        y = fromIntegral $ (i `mod` 100) - 50 :: GLfloat
        m44 = mkTransformation 
                (axisAngle (V3 1 1 0) 1) 
                (V3 x (y + sin (t + fromIntegral i)) 0)
    return (transpose m44 :: M44 GLfloat)

generateColors :: GLsizei -> GLfloat -> IO [V4 GLfloat]
generateColors instanceCount t = forM [0..instanceCount-1] $ \i -> do
    hue <- randomIO
    --return (hslColor (fromIntegral i / fromIntegral instanceCount) 0.9 0.6)
    return (hslColor hue 0.9 0.6)

main :: IO ()
main = do
    (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768
  
    shader        <- createShaderProgram "test/geoInstancedM44.vert" "test/geoInstancedM44.frag"
  
    cubeGeo       <- cubeGeometry 0.5 1
    cubeShape     <- makeShape cubeGeo shader
  
    let numInstances = 10000
    initialTransforms  <- generateTransforms numInstances 0
    initialColors      <- generateColors numInstances 0
    positionsBuffer    <- bufferData GL_DYNAMIC_DRAW (concatMap toList initialTransforms)
    colorsBuffer       <- bufferData GL_DYNAMIC_DRAW (concatMap toList initialColors)
    withShape cubeShape $ do
        withArrayBuffer positionsBuffer $ 
            assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT
        withArrayBuffer colorsBuffer $ 
            assignFloatAttributeInstanced shader "aInstanceColor" GL_FLOAT 4
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
        newM44s   <- generateTransforms numInstances t
        newColors <- generateColors numInstances t
        bufferSubData positionsBuffer (concatMap toList newM44s)
        bufferSubData colorsBuffer    (concatMap toList newColors)
        
        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawShapeInstanced numInstances
            
        swapBuffers win

