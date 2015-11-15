module Graphics.GL.Pal.AssignAttribute where

import Foreign
import Graphics.GL
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Shader
import Control.Monad.Trans
import Control.Monad

integerTypes :: [GLenum]
integerTypes = [GL_BYTE, GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_SHORT]

enableVertexAttribArray :: AttributeLocation -> IO ()
enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral . unAttributeLocation

assignFloatAttribute :: Program -> String -> GLenum -> GLint -> IO ()
assignFloatAttribute = assignFloatAttribute' False

assignFloatAttributeInstanced :: Program -> String -> GLenum -> GLint -> IO ()
assignFloatAttributeInstanced = assignFloatAttribute' True

assignFloatAttribute' :: Bool -> Program -> String -> GLenum -> GLint -> IO ()
assignFloatAttribute' instanced prog attributeName attributeType attributeLength = do

  when (attributeType `elem` integerTypes) $ 
    putStrLn $ "WARNING: Passed integer type " ++ show attributeType ++ " to assignFloatAttribute."

  -- Gets the attribute for the program we have passed in
  attribute <- getShaderAttribute prog attributeName

  -- Describe our array to OpenGL
  enableVertexAttribArray attribute

  glVertexAttribPointer
    (fromIntegral (unAttributeLocation attribute)) -- attribute
    attributeLength   -- number of elements per vertex, e.g. (x,y,z)
    attributeType     -- the type of each element
    GL_FALSE          -- don't normalize
    0                 -- no extra data between each position
    nullPtr           -- offset of first element

  when instanced $ 
    vertexAttribDivisor attribute 1


assignIntegerAttribute :: Program -> String -> GLenum -> GLint -> IO ()
assignIntegerAttribute = assignIntegerAttribute' False

assignIntegerAttributeInstanced :: Program -> String -> GLenum -> GLint -> IO ()
assignIntegerAttributeInstanced = assignIntegerAttribute' True

assignIntegerAttribute' :: Bool -> Program -> String -> GLenum -> GLint -> IO ()
assignIntegerAttribute' instanced prog attributeName attributeType attributeLength = do

  when (not $ attributeType `elem` integerTypes) $ 
    putStrLn $ "WARNING: Passed non-integer type " ++ show attributeType ++ " to assignIntegerAttribute."

  -- Gets the attribute for the program we have passed in
  attribute <- getShaderAttribute prog attributeName

  -- Describe our array to OpenGL
  enableVertexAttribArray attribute

  glVertexAttribIPointer
    (fromIntegral (unAttributeLocation attribute)) -- attribute
    attributeLength   -- number of elements per vertex, e.g. (x,y,z)
    attributeType     -- the type of each element
    0                 -- no extra data between each position
    nullPtr           -- offset of first element

  when instanced $ 
    vertexAttribDivisor attribute 1

vertexAttribDivisor :: MonadIO m => AttributeLocation -> GLuint -> m ()
vertexAttribDivisor attribute = glVertexAttribDivisor (fromIntegral (unAttributeLocation attribute))
