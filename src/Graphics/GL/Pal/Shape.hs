{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.GL.Pal.Shape where
  
import Graphics.GL

import Control.Monad.Reader
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Utility
import Graphics.GL.Pal.InferUniforms
import Graphics.GL.Pal.WithActions
import Graphics.GL.Pal.AssignAttribute
import Graphics.GL.Pal.ArrayBuffer
import Graphics.GL.Pal.Shader
import Data.Data

import Foreign

newVAO :: MonadIO m => m VertexArrayObject
newVAO = VertexArrayObject <$> overPtr (glGenVertexArrays 1)

-- | A shape is the combination of a VAO, a program, 
-- and the collection of uniforms for that program.
makeShape :: (MonadIO m, Data u) => Geometry -> Program -> m (Shape u)
makeShape sGeometry@Geometry{..} sProgram = do

    -- Setup a VAO
    sVAO <- newVAO
  
    withVAO sVAO $ do
  
        withArrayBuffer geoPositions $ assignFloatAttribute sProgram "aPosition" GL_FLOAT 3 
        withArrayBuffer geoNormals   $ assignFloatAttribute sProgram "aNormal"   GL_FLOAT 3 
        withArrayBuffer geoTangents  $ assignFloatAttribute sProgram "aTangent"  GL_FLOAT 3
        withArrayBuffer geoUVs       $ assignFloatAttribute sProgram "aUV"       GL_FLOAT 2 
    
        bindElementArrayBuffer geoIndices
  
    sUniforms <- acquireUniforms sProgram
  
    return Shape{..}
  
withShape :: MonadIO m => Shape t -> ReaderT (Shape t) m a -> m a
withShape shape@Shape{..} action = do
    useProgram sProgram
    withVAO sVAO (runReaderT action shape)

-- | Must be called from within withShape
drawShape :: (MonadReader (Shape u) m, MonadIO m) => m ()
drawShape = do
    Shape{..} <- ask
    let vertCount = geoVertCount sGeometry
    glDrawElements GL_TRIANGLES vertCount GL_UNSIGNED_INT nullPtr

drawShapeInstanced :: (MonadReader (Shape u) m, MonadIO m) => GLsizei -> m ()
drawShapeInstanced instanceCount = do
    Shape{..} <- ask
    let vertCount = geoVertCount sGeometry
    glDrawElementsInstanced GL_TRIANGLES vertCount GL_UNSIGNED_INT nullPtr instanceCount


drawShapeInstancedBaseInstance :: (MonadReader (Shape u) m, MonadIO m) => GLsizei -> GLuint -> m ()
drawShapeInstancedBaseInstance instanceCount baseInstance = do
    Shape{..} <- ask
    let vertCount = geoVertCount sGeometry
    glDrawElementsInstancedBaseInstance GL_TRIANGLES vertCount GL_UNSIGNED_INT nullPtr instanceCount baseInstance