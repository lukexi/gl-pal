{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable

import System.Random

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) 
  } deriving Data

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  shader        <- createShaderProgram "test/geo.vert" "test/geo.frag"
  Uniforms{..}  <- acquireUniforms shader

  icoGeo     <- icosahedronGeometry 0.5 5
  icoShape   <- makeShape icoGeo shader

  cubeGeo    <- cubeGeometry (V3 2 1 1) 5
  cubeShape  <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
  
  planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
  planeShape <- makeShape planeGeo shader

  let shapes = [ (cubeShape,  V3 (-1) 0 0, (V3 (-1)   (-0.5) (-0.5), V3 3 0.5 0.5))
               , (icoShape ,  V3 1 0 0   , (V3 (-0.5) (-0.5) (-0.5), V3 0.5 0.5 0.5))
               , (planeShape, V3 0 (-1) 0, (V3 (-0.5) (-0.5) 0     , V3 0.5 0.5 0))
               ]

  (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

  glEnable GL_DEPTH_TEST

  whileWindow win $ do
    proj44 <- getWindowProjection win 45 0.1 1000
    let pose = Pose (V3 0 0 5) (axisAngle (V3 0 1 0) 0)
        view44 = viewMatrixFromPose pose
    (x,y,w,h) <- getWindowViewport win
    glViewport x y w h

    t <- realToFrac . utctDayTime <$> getCurrentTime

    processEvents events $ \e -> do
      closeOnEscape win e
      onMouseDown e $ \_ -> do
        ray <- cursorPosToWorldRay win proj44 pose
        forM_ (zip ["cube", "sphere", "plane"] shapes) $ \(name, (_shape, pos, aabb)) -> do
          let model44 = mkTransformation (axisAngle (V3 0 1 0) (pi/4)) pos
              intersection = rayOBBIntersection ray aabb model44
          putStrLn $ name ++ ": " ++ show intersection
        putStrLn ""

    glClearColor 0.0 0.0 0.1 1
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    forM_ shapes $ \(shape, pos, _aabb) -> 
      withShape shape $ do
        let model44 = mkTransformation (axisAngle (V3 0 1 0) (pi/4)) pos
        uniformM44 uMVP (proj44 !*! view44 !*! model44)
        drawShape

    newVerts <- randomVerts lineVertCount
    bufferSubData lineBuffer (concatMap toList newVerts)
    

    let model44 = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 1 0)
    uniformM44 uMVP (proj44 !*! view44 !*! model44)
    withVAO lineVAO $ 
      glDrawArrays GL_LINE_STRIP 0 lineVertCount

    swapBuffers win

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = map (\x -> V3 x 0 0) [-1,-0.95..1] :: [V3 GLfloat]
      vertCount = length verts
      normals = replicate vertCount (V3 0 0 1)   :: [V3 GLfloat]
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts)
  normalsBuffer   <- bufferData GL_STATIC_DRAW (concatMap toList normals)

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
    withArrayBuffer normalsBuffer   $ assignFloatAttribute shader "aNormal"   GL_FLOAT 3

  return (vao, positionsBuffer, fromIntegral vertCount)

randomVerts :: (Integral a) 
            => a -> IO [V3 GLfloat]
randomVerts lineVertCount = forM [0..lineVertCount-1] $ \i -> do
  let x = fromIntegral i / fromIntegral lineVertCount
      x' = x * 2 - 1
  y <- randomIO
  return (V3 x' y 0)
