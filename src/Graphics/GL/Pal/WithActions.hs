module Graphics.GL.Pal.WithActions where

import Graphics.GL
import Control.Monad.Trans

import Graphics.GL.Pal.Types

withVAO :: MonadIO m => VertexArrayObject -> m a -> m a
withVAO aVAO action = do

  glBindVertexArray (unVertexArrayObject aVAO)

  result <- action

  glBindVertexArray 0

  return result


withArrayBuffer :: MonadIO m => ArrayBuffer -> m a -> m a
withArrayBuffer buffer action = do

  glBindBuffer GL_ARRAY_BUFFER (unArrayBuffer buffer)

  result <- action
  
  glBindBuffer GL_ARRAY_BUFFER 0

  return result

withElementArrayBuffer :: MonadIO m => ElementArrayBuffer -> m a -> m a
withElementArrayBuffer buffer action = do

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER (unElementArrayBuffer buffer)

  result <- action
  
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

  return result


withUniformBuffer :: MonadIO m => UniformBuffer -> m a -> m a
withUniformBuffer buffer action = do

  glBindBuffer GL_UNIFORM_BUFFER (unUniformBuffer buffer)

  result <- action
  
  glBindBuffer GL_UNIFORM_BUFFER 0

  return result
