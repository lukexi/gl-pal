{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import SDL.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable

-- import System.Random

data Uniforms = Uniforms 
    { uMVP :: UniformLocation (M44 GLfloat)
    } deriving Data

randomPositions :: GLsizei -> GLfloat -> IO [V3 GLfloat]
randomPositions instanceCount t = forM [0..instanceCount-1] $ \i -> do
    let x = fromIntegral $ (i `div` 100) - 50
        y = fromIntegral $ (i `mod` 100) - 50
    return (V3 x (y + sin (t + fromIntegral i)) 0)

main :: IO ()
main = do
    win <- reacquire 0 $ createGLWindow "Geometry Test"
  
    shader        <- createShaderProgram "test/geoInstanced.vert" "test/geo.frag"
  
    cubeGeo       <- cubeGeometry 0.5 1
    cubeShape     <- makeShape cubeGeo shader
  
    let numInstances = 1000
  
    initialOffsets  <- randomPositions numInstances 0
    positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList initialOffsets)
    --iBuffer        <- bufferData GL_DYNAMIC_DRAW (replicate (fromIntegral numInstances) 5 :: [GLint])
    withShape cubeShape $ do
        withArrayBuffer positionsBuffer $ 
            assignFloatAttributeInstanced   shader "aInstancePosition" GL_FLOAT 3
        --withArrayBuffer iBuffer $
        --    assignIntegerAttributeInstanced shader "aInstancePosI"     GL_INT   1
    
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
  
    whileWindow win $ \events -> do

        projection <- getWindowProjection win 45 0.1 1000
        (x,y,w,h)  <- getWindowViewport win
        glViewport x y w h
        
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        
        let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)
    
        t <- (*10) . realToFrac . utctDayTime <$> getCurrentTime
    
        newOffsets <- randomPositions numInstances t
        bufferSubData positionsBuffer (concatMap toList newOffsets)
        --bufferSubData iBuffer (replicate (fromIntegral numInstances) 0 :: [GLint])
        
        let model = mkTransformation (axisAngle (V3 1 1 0) 1) (V3 0 1 0)
    
        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uMVP (projection !*! view !*! model)
            drawShapeInstanced numInstances
    
        glSwapWindow win

