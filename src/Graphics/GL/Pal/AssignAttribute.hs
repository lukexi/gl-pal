module Graphics.GL.Pal.AssignAttribute where

import Foreign
import Graphics.GL
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Shader
import Control.Monad.Trans
import Control.Monad
import Linear

integerTypes :: [GLenum]
integerTypes = [GL_BYTE, GL_UNSIGNED_BYTE, GL_SHORT, GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_SHORT]

enableVertexAttribArray :: MonadIO m => AttributeLocation -> m ()
enableVertexAttribArray = glEnableVertexAttribArray . fromIntegral . unAttributeLocation

assignFloatAttribute :: MonadIO m => Program -> String -> GLenum -> GLint -> m ()
assignFloatAttribute = assignFloatAttribute' False

assignFloatAttributeInstanced :: MonadIO m => Program -> String -> GLenum -> GLint -> m ()
assignFloatAttributeInstanced = assignFloatAttribute' True

assignFloatAttribute' :: MonadIO m => Bool -> Program -> String -> GLenum -> GLint -> m ()
assignFloatAttribute' instanced prog attributeName attributeType attributeLength = do
  
    when (attributeType `elem` integerTypes) $ 
        liftIO . putStrLn $ "WARNING: Passed integer type " ++ show attributeType ++ " to assignFloatAttribute."
  
    -- Gets the attribute for the program we have passed in
    attribute <- getShaderAttribute prog attributeName
    liftIO (print (attributeName, attribute))
  
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
  

assignMatrixAttributeInstanced :: MonadIO m => Program -> String -> GLenum -> m ()
assignMatrixAttributeInstanced prog attributeName attributeType = do
  
    when (attributeType `elem` integerTypes) $ 
        liftIO . putStrLn $ "WARNING: Passed integer type " ++ show attributeType ++ " to assignFloatAttribute."
  
    -- Gets the attribute for the program we have passed in
    baseAttribute <- getShaderAttribute prog attributeName
    liftIO (print (attributeName, baseAttribute))
  
    -- Describe our array to OpenGL
    
    
    -- mat4s take up 4 slots
    let v4Size = fromIntegral (sizeOf (undefined :: V4 GLfloat))
    forM_ [0..3] $ \locationOffset -> do
        let attribute = unAttributeLocation baseAttribute + locationOffset
        enableVertexAttribArray (AttributeLocation attribute)
        glVertexAttribPointer (fromIntegral attribute)
            4                 -- number of elements per vertex, in this case one row of the matrix
            attributeType     -- the type of each element
            GL_FALSE          -- don't normalize
            (v4Size * 4)            -- no extra data between each position
            (nullPtr `plusPtr` (fromIntegral v4Size * (fromIntegral locationOffset))) -- offset of first element

        vertexAttribDivisor (AttributeLocation attribute) 1
  

assignIntegerAttribute :: MonadIO m => Program -> String -> GLenum -> GLint -> m ()
assignIntegerAttribute = assignIntegerAttribute' False

assignIntegerAttributeInstanced :: MonadIO m => Program -> String -> GLenum -> GLint -> m ()
assignIntegerAttributeInstanced = assignIntegerAttribute' True

assignIntegerAttribute' :: MonadIO m => Bool -> Program -> String -> GLenum -> GLint -> m ()
assignIntegerAttribute' instanced prog attributeName attributeType attributeLength = do

    when (not $ attributeType `elem` integerTypes) $ 
        liftIO . putStrLn $ "WARNING: Passed non-integer type " ++ show attributeType ++ " to assignIntegerAttribute."
  
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
