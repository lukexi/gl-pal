{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time

data Uniforms = Uniforms { uMVP :: UniformLocation (M44 GLfloat) } deriving Data

main :: IO ()
main = do
  (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768

  icoProg   <- createShaderProgram "test/geo.vert" "test/geo.frag"
  icoGeo    <- icosahedronGeometry 0.5 5
  icoShape  <- makeShape icoGeo icoProg

  cubeProg   <- createShaderProgram "test/geo.vert" "test/geo.frag"
  cubeGeo    <- cubeGeometry 1 5
  cubeShape  <- makeShape cubeGeo cubeProg

  let shapes = [ (cubeShape, V3 (-1) 0 0)
               , (icoShape , V3 1 0 0)
               ]

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
        Uniforms{..} <- asks sUniforms
        uniformM44 uMVP (projection !*! view !*! model)
        drawShape

    swapBuffers win
