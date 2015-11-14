{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time
import Data.Foldable
import Foreign.C.String
import System.Random

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat)
  } deriving Data

randomOffsets :: (Integral a, Fractional b, Floating b, Random b) 
              => a -> b -> IO [V4 b]
randomOffsets instanceCount t = forM [0..instanceCount-1] $ \i -> do
  let x = fromIntegral $ (i `div` 100) - 50
      y = fromIntegral $ (i `mod` 100) - 50
  return (V4 x (y + sin (t + fromIntegral i)) 0 0)

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  shader        <- createShaderProgram "test/geoInstancedUniformBuffer.vert" "test/geo.frag"

  cubeGeo    <- cubeGeometry 0.5 1
  cubeShape  <- makeShape cubeGeo shader

  let numInstances = 10000

  initialOffsets <- randomOffsets numInstances 0
  offsetBuffer   <- bufferUniformData GL_DYNAMIC_DRAW (concatMap toList initialOffsets)

  -- Set up our UBO globally
  let offsetInfoBindingPoint = 0
  glBindBufferBase GL_UNIFORM_BUFFER offsetInfoBindingPoint (unUniformBuffer offsetBuffer)

  -- Bind the shader's uniform buffer declaration to the correct uniform buffer object
  let uniformBlockName = "offsetInfo"
  shaderOffsetInfoBlockIndex <- withCString uniformBlockName $ \uniformBlockNameCString -> 
    glGetUniformBlockIndex (unProgram shader) uniformBlockNameCString
  glUniformBlockBinding (unProgram shader) shaderOffsetInfoBlockIndex offsetInfoBindingPoint

  -- withVAO (sVAO cubeShape) $ 
  --   withArrayBuffer offsetBuffer $ do
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
    newOffsets <- randomOffsets numInstances t
    bufferUniformSubData offsetBuffer (concatMap toList newOffsets)
    
    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 1 0)

    withShape cubeShape $ do
      Uniforms{..} <- asks sUniforms
      uniformM44 uMVP (projection !*! view !*! model)
      drawShapeInstanced numInstances

    swapBuffers win

