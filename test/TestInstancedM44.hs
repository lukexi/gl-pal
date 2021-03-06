{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import SDL.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time

import qualified Data.Vector.Storable.Mutable as VM

data Uniforms = Uniforms
    { uProjectionView :: UniformLocation (M44 GLfloat)
    } deriving Data

numInstances :: Num a => a
numInstances = 100

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
    where hue = fromIntegral i / numInstances + sin t

loopM :: (Monad m) => Int -> (Int -> m a) -> m a
loopM n action = go 0
    where
        go !i
            | i == (n-1) = action i
            | otherwise  = action i >> go (i + 1)

main :: IO ()
main = do
    win <- reacquire 0 $ createGLWindow "Geometry Test"

    shader        <- createShaderProgram "test/geoInstancedM44.vert" "test/geoInstancedM44.frag"

    cubeGeo       <- cubeGeometry 0.5 1
    cubeShape     <- makeShape cubeGeo shader

    transformsVector <- VM.replicate numInstances (identity :: M44 GLfloat)
    colorsVector     <- VM.replicate numInstances (V4 0 0 0 0 :: V4 GLfloat)
    transformsBuffer <- bufferDataV GL_DYNAMIC_DRAW transformsVector
    colorsBuffer     <- bufferDataV GL_DYNAMIC_DRAW colorsVector

    withShape cubeShape $ do
        withArrayBuffer transformsBuffer $
            assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT
        withArrayBuffer colorsBuffer $
            assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1

    whileWindow win $ do
        liftIO performMinorGC


        projection <- getWindowProjection win 45 0.1 1000
        (x,y,w,h)  <- getWindowViewport win
        glViewport x y w h

        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)

        t <- (*10) . realToFrac . utctDayTime <$> getCurrentTime

        profileMS ("writeVectors") 0 $ do
            loop numInstances (\i -> VM.write transformsVector i (generateTransforms t i))
            loop numInstances (\i -> VM.write colorsVector i (generateColors t i))

        bufferSubDataV transformsBuffer transformsVector
        bufferSubDataV colorsBuffer    colorsVector

        withShape cubeShape $ do
            Uniforms{..} <- asks sUniforms
            uniformM44 uProjectionView (projection !*! view)
            drawShapeInstanced numInstances

        glSwapWindow win

