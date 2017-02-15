{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified SDL.GLFW as GLFW
import Graphics.GL

import Data.Bits
import Control.Monad
import Linear

import SetupGLFW
import Graphics.GL.Pal
import Cube

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

resX, resY :: Num a => a
resX = 1920
resY = 1080

main :: IO a
main = do

    win      <- createGLWindow "GL Pal"

    cubeProg <- createReshaderProgram "test/cube.vert" "test/cube.frag"
    
    cube     <- makeCube =<< cubeProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    whileWindow win $ \events -> do

        useProgram =<< cubeProg

        -- Clear the framebuffer
        glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

        -- Render our scene
        let projection = perspective 45 (resX/resY) 0.01 1000
            model      = mkTransformation 1 (V3 0 0 (-4))
            view       = lookAt (V3 0 2 5) (V3 0 0 (-4)) (V3 0 1 0)
            mvp        = projection !*! view !*! model
            (x,y,w,h)  = (0,0,1920,1080)

        glViewport x y w h

        renderCube cube mvp

        glSwapWindow win


