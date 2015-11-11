{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable

import System.Random

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) } 
  deriving Data

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = map (\x -> V3 x 0 0) [-1,-0.95..1]
      vertCount = length verts
      normals = replicate vertCount (V3 0 0 1)
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts)
  normalsBuffer   <- bufferData GL_STATIC_DRAW (concatMap toList normals)

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignAttribute shader "aPosition" 3
    withArrayBuffer normalsBuffer $ assignAttribute shader "aNormal" 3

  return (vao, positionsBuffer, fromIntegral vertCount)

randomVerts :: (Integral a, Fractional b, Random b) 
            => a -> IO [V3 b]
randomVerts lineVertCount = forM [0..lineVertCount-1] $ \i -> do
  let x = fromIntegral i / fromIntegral lineVertCount
      x' = x * 2 - 1
  y <- randomIO
  return (V3 x' y 0)

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  shader        <- createShaderProgram "test/geo.vert" "test/geo.frag"
  Uniforms{..}  <- acquireUniforms shader

  icoGeo     <- icosahedronGeometry 0.5 5
  icoShape   <- makeShape icoGeo shader

  cubeGeo    <- cubeGeometry 1 5
  cubeShape  <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
  
  planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
  planeShape <- makeShape planeGeo shader

  let shapes = [ (cubeShape, V3 (-1) 0 0)
               , (icoShape , V3 1 0 0)
               , (planeShape, V3 0 (-1) 0)
               ]

  (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

  glEnable GL_DEPTH_TEST

  whileWindow win $ do
    processEvents events $ closeOnEscape win

    t <- realToFrac . utctDayTime <$> getCurrentTime

    glClearColor 0.0 0.0 0.1 1
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    projection <- windowProjection win 45 0.1 1000
    let view = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)

    forM_ shapes $ \(shape, pos) -> 
      withShape shape $ do
        let model = mkTransformation (axisAngle (V3 1 1 0) t) pos
        uniformM44 uMVP (projection !*! view !*! model)
        drawShape

    newVerts <- randomVerts lineVertCount
    bufferSubData lineBuffer (concatMap toList newVerts)
    

    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 1 0)
    uniformM44 uMVP (projection !*! view !*! model)
    withVAO lineVAO $ 
      glDrawArrays GL_LINE_STRIP 0 lineVertCount

    swapBuffers win
