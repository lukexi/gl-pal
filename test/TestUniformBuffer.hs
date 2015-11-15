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

generateBlocks :: Int -> GLfloat -> IO [GLfloat]
generateBlocks instanceCount t = fmap concat $ forM [0..instanceCount-1] $ \i -> do
  let x = fromIntegral $ (i `div` 100) - 50
      y = fromIntegral $ (i `mod` 100) - 50
      ii = fromIntegral i / fromIntegral instanceCount
      offset = V4 x (y + sin (t + fromIntegral i)) 0 0
      color = hslColor ii 0.9 0.9 1
  return $ toList offset ++ toList color

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  shader     <- createShaderProgram 
    "test/geoInstancedUniformBuffer.vert" 
    "test/geoInstancedUniformBuffer.frag" 

  cubeGeo    <- cubeGeometry 0.5 1
  cubeShape  <- makeShape cubeGeo shader

  let numInstances = 10000

  initialBlocks <- generateBlocks numInstances 0
  uniformBlockBuffer  <- bufferUniformData GL_DYNAMIC_DRAW initialBlocks

  -- Set up our UBO globally
  let uniformBlockBindingPoint = UniformBlockBindingPoint 0
  bindUniformBufferBase uniformBlockBuffer uniformBlockBindingPoint

  -- Bind the shader's uniform buffer declaration to the correct uniform buffer object
  bindShaderUniformBuffer shader "myUniformBlock" uniformBlockBindingPoint

  -- withVAO (sVAO cubeShape) $ 
  --   withArrayBuffer offsetsBuffer $ do
  --     attribute <- getShaderAttribute (sProgram cubeShape) "aInstanceOffset"
  --     assignAttribute (sProgram cubeShape) "aInstanceOffset" 3
  --     glVertexAttribDivisor (fromIntegral (unAttributeLocation attribute)) 1

  glEnable GL_DEPTH_TEST
  glClearColor 0.0 0.0 0.1 1

  whileWindow win $ do
    projection <- getWindowProjection win 45 0.1 1000
    (x,y,w,h)  <- getWindowViewport win
    glViewport x y w h
    let view = viewMatrix (V3 0 0 100) (axisAngle (V3 0 1 0) 0)

    processEvents events $ closeOnEscape win
    
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    t <- (*10) . realToFrac . utctDayTime <$> getCurrentTime

    newBlocks <- generateBlocks numInstances t
    bufferUniformSubData uniformBlockBuffer newBlocks
    
    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 1 0)

    withShape cubeShape $ do
      Uniforms{..} <- asks sUniforms
      uniformM44 uMVP (projection !*! view !*! model)
      drawShapeInstanced (fromIntegral numInstances)

    swapBuffers win

