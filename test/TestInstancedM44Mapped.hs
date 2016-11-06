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

data Uniforms = Uniforms
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

maxInstances :: Num a => a
maxInstances = 10000

streamingBufferCapacity :: Int
streamingBufferCapacity = maxInstances * 16

generateTransforms :: Integral a => GLfloat -> a -> M44 GLfloat
generateTransforms t i =
    let x = fromIntegral $ (i `div` 100) - 50 :: GLfloat
        y = fromIntegral $ (i `mod` 100) - 50 :: GLfloat
        m44 = mkTransformation
                (axisAngle (V3 1 1 0) 1)
                (V3 x (y + sin (t + fromIntegral i)) 0)
    in m44

generateColors :: Integral a => GLfloat -> a -> V4 GLfloat
generateColors t i = colorHSL hue 0.9 0.6
    where hue = fromIntegral i / maxInstances + sin t



main :: IO ()
main = do
    (win, _, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

    shader        <- createShaderProgram "test/geoInstancedM44.vert" "test/geoInstancedM44.frag"

    cubeGeo       <- cubeGeometry 0.5 1
    cubeShape     <- makeShape cubeGeo shader


    sab              <- makeSAB streamingBufferCapacity
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
        events <- gatherEvents events
        forM_ events (closeOnEscape win)

        projection <- getWindowProjection win 45 0.1 1000
        (x,y,w,h)  <- getWindowViewport win
        glViewport x y w h

        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)

        t <- (*3) . realToFrac . utctDayTime <$> getCurrentTime

        writeSAB sab maxInstances resetShapeInstanceBuffers $ do
            fillSABBuffer transformsBuffer  (return . generateTransforms t)
            fillSABBuffer colorsBuffer      (return . generateColors t)

        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawSAB sab maxInstances

        swapBuffers win


