{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.GL.Pal.Shape where
  
import Graphics.GL

import Control.Monad.Reader
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.InferUniforms
import Graphics.GL.Pal.WithActions
import Graphics.GL.Pal.AssignAttribute
import Graphics.GL.Pal.Shader
import Data.Data

import Foreign

-- | A shape is the combination of a VAO, a program, 
-- and the collection of uniforms for that program.
makeShape :: Data u => Geometry -> Program -> IO (Shape u)
makeShape sGeometry@Geometry{..} sProgram = do

  -- Setup a VAO
  sVAO <- VertexArrayObject <$> overPtr ( glGenVertexArrays 1 )

  withVAO sVAO $ do

    withArrayBuffer geoPositions $ assignAttribute sProgram "aPosition" 3 
    withArrayBuffer geoNormals   $ assignAttribute sProgram "aNormal"   3 
    withArrayBuffer geoTangents  $ assignAttribute sProgram "aTangent"  3
    withArrayBuffer geoUVs       $ assignAttribute sProgram "aUV"       2 

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER (unElementArrayBuffer geoIndices)


  sUniforms <- acquireUniforms sProgram

  return Shape{..}


  
withShape :: MonadIO m => Shape t -> ReaderT (Shape t) m a -> m ()
withShape shape@Shape{..} action = do
  useProgram sProgram
  withVAO sVAO (runReaderT action shape)

-- | Must be called from within withShape
drawShape :: (MonadReader (Shape u) m, MonadIO m) => m ()
drawShape = do
  Shape{..} <- ask
  let vc = geoVertCount sGeometry
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr
