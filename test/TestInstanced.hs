{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable

-- import System.Random

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat)
  } deriving Data

randomOffsets :: GLsizei -> GLfloat -> IO [V3 GLfloat]
randomOffsets instanceCount t = forM [0..instanceCount-1] $ \i -> do
  let x = fromIntegral $ (i `div` 100) - 50
      y = fromIntegral $ (i `mod` 100) - 50
  return (V3 x (y + sin (t + fromIntegral i)) 0)

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  shader        <- createShaderProgram "test/geoInstanced.vert" "test/geo.frag"

  cubeGeo       <- cubeGeometry 0.5 1
  cubeShape     <- makeShape cubeGeo shader

  let numInstances = 10000

  initialOffsets <- randomOffsets numInstances 0
  offsetBuffer   <- bufferData GL_DYNAMIC_DRAW (concatMap toList initialOffsets)
  iBuffer        <- bufferData GL_DYNAMIC_DRAW (replicate (fromIntegral numInstances) 5 :: [GLint])
  withVAO (sVAO cubeShape) $ do
    withArrayBuffer offsetBuffer $ 
      assignFloatAttributeInstanced program "aInstanceOffset " GL_FLOAT 3
    withArrayBuffer iBuffer $
      assignIntegerAttributeInstanced program "aInstanceI"     GL_INT   1


  glEnable GL_DEPTH_TEST
  glClearColor 0.0 0.0 0.1 1

  whileWindow win $ do
    projection <- getWindowProjection win 45 0.1 1000
    (x,y,w,h)  <- getWindowViewport win
    glViewport x y w h
    let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)

    t <- (*10) . realToFrac . utctDayTime <$> getCurrentTime

    processEvents events $ closeOnEscape win
    
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    newOffsets <- randomOffsets numInstances t
    bufferSubData offsetBuffer (concatMap toList newOffsets)
    -- bufferSubData iBuffer (replicate (fromIntegral numInstances) 0 :: [GLint])
    
    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 1 0)

    withShape cubeShape $ do
      Uniforms{..} <- asks sUniforms
      uniformM44 uMVP (projection !*! view !*! model)
      drawShapeInstanced numInstances

    swapBuffers win

