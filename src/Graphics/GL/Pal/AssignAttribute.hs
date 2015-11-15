module Graphics.GL.Pal.AssignAttribute where

import Foreign
import Graphics.GL
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Shader
import Control.Monad.Trans

assignAttribute :: Program -> String -> GLenum -> GLint -> IO ()
assignAttribute prog attributeName attributeType attributeLength = do

  -- Gets the attribute for the program we have passed in
  attribute <- getShaderAttribute prog attributeName

  -- Describe our array to OpenGL
  glEnableVertexAttribArray (fromIntegral (unAttributeLocation attribute))

  glVertexAttribPointer
    (fromIntegral (unAttributeLocation attribute)) -- attribute
    attributeLength   -- number of elements per vertex, e.g. (x,y,z)
    attributeType     -- the type of each element
    GL_FALSE          -- don't normalize
    0                 -- no extra data between each position
    nullPtr           -- offset of first element

vertexAttribDivisor :: MonadIO m => AttributeLocation -> GLuint -> m ()
vertexAttribDivisor attribute = glVertexAttribDivisor (fromIntegral (unAttributeLocation attribute))
