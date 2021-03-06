{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import SDL.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time

import System.Random



data Uniforms = Uniforms 
    { uMVP :: UniformLocation (M44 GLfloat) 
    } deriving Data

main :: IO ()
main = do
    win <- reacquire 0 $ createGLWindow "Geometry test"
  
    shader        <- createShaderProgram "test/geo.vert" "test/geo.frag"
    Uniforms{..}  <- acquireUniforms shader
  
    octaGeo     <- octahedronGeometry 0.5 0
    octaShape   <- makeShape octaGeo shader

    tetraGeo     <- tetrahedronGeometry 0.5
    tetraShape   <- makeShape tetraGeo shader
    
    cubeGeo    <- cubeGeometry 1 5
    cubeShape  <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
    
    planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
    planeShape <- makeShape planeGeo shader
    
    let shapes = [ (cubeShape, V3 (-1) (-2) 0)
                 , (octaShape , V3 (-1) 0 0)
                 , (planeShape, V3 1 (-2) 0)
                 , (tetraShape, V3 1 0 0)
                 ]
  
    (lineVAO, lineBuffer, lineVertCount) <- makeLine shader
  
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1

    -- Wireframe
    --glPolygonMode GL_FRONT_AND_BACK GL_LINE
  
    whileWindow win $ \events -> do
        projection <- getWindowProjection win 45 0.1 1000
        let view = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)
        (x,y,w,h) <- getWindowViewport win
        glViewport x y w h
        
    
        t <- realToFrac . utctDayTime <$> getCurrentTime
    
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
        
    
        forM_ shapes $ \(shape, pos) -> 
            withShape shape $ do
                let model = mkTransformation (axisAngle (V3 1 1 0) t) pos
                uniformM44 uMVP (projection !*! view !*! model)
                drawShape
    
        newVerts <- randomVerts lineVertCount
        bufferSubData lineBuffer (newVerts :: [V3 GLfloat])
        
    
        let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 1 0)
        uniformM44 uMVP (projection !*! view !*! model)
        withVAO lineVAO $ 
            glDrawArrays GL_LINE_STRIP 0 lineVertCount
    
        glSwapWindow win

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer (V3 GLfloat), GLsizei)
makeLine shader = do

    let verts = map (\x -> V3 x 0 0) [-1,-0.95..1]
        vertCount = length verts
        normals = replicate vertCount (V3 0 0 1)
    
    positionsBuffer <- bufferData GL_DYNAMIC_DRAW (verts :: [V3 GLfloat])
    normalsBuffer   <- bufferData GL_STATIC_DRAW (normals :: [V3 GLfloat])
  
    vao <- newVAO
    withVAO vao $ do
        withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
        withArrayBuffer normalsBuffer   $ assignFloatAttribute shader "aNormal"   GL_FLOAT 3
  
    return (vao, positionsBuffer, fromIntegral vertCount)

randomVerts :: (Integral a, Fractional b, Random b) 
            => a -> IO [V3 b]
randomVerts lineVertCount = forM [0..lineVertCount-1] $ \i -> do
    let x = fromIntegral i / fromIntegral lineVertCount
        x' = x * 2 - 1
    y <- randomIO
    return (V3 x' y 0)











