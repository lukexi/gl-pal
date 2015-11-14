module Graphics.GL.Pal.ArrayBuffer where

import Foreign
import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.WithActions

import Control.Monad.Trans

newUniformBuffer :: MonadIO m => m UniformBuffer
newUniformBuffer = UniformBuffer <$> overPtr (glGenBuffers 1)

bindUniformBufferBase :: MonadIO m => UniformBuffer -> UniformBlockBindingPoint -> m ()
bindUniformBufferBase buffer bindingPoint = 
  glBindBufferBase GL_UNIFORM_BUFFER 
    (unUniformBlockBindingPoint bindingPoint) 
    (unUniformBuffer buffer)


newArrayBuffer :: MonadIO m => m ArrayBuffer
newArrayBuffer = ArrayBuffer <$> overPtr (glGenBuffers 1)

newElementArrayBuffer :: MonadIO m => m ElementArrayBuffer
newElementArrayBuffer = ElementArrayBuffer <$> overPtr (glGenBuffers 1)

-- | Buffers a list of floats using the given draw hint, e.g. GL_STATIC_DRAW
-- and returns a reference to the ArrayBuffer where the data is stored
bufferData :: MonadIO m => GLenum -> [GLfloat] -> m ArrayBuffer
bufferData drawType values = do

  buffer <- newArrayBuffer

  withArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    liftIO . withArray values $ 
      \valuesPtr ->
        glBufferData GL_ARRAY_BUFFER valuesSize (castPtr valuesPtr) drawType

  return buffer


bufferUniformData :: MonadIO m => GLenum -> [GLfloat] -> m UniformBuffer
bufferUniformData drawType values = do

  buffer <- newUniformBuffer

  withUniformBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    liftIO . withArray values $ 
      \valuesPtr ->
        glBufferData GL_UNIFORM_BUFFER valuesSize (castPtr valuesPtr) drawType

  return buffer

bufferSubData :: MonadIO m => ArrayBuffer -> [GLfloat] -> m ()
bufferSubData buffer values = do

  withArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    liftIO . withArray values $ 
      \valuesPtr ->
        glBufferSubData GL_ARRAY_BUFFER 0 valuesSize (castPtr valuesPtr)

bufferUniformSubData :: MonadIO m => UniformBuffer -> [GLfloat] -> m ()
bufferUniformSubData buffer values = do

  withUniformBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    liftIO . withArray values $ 
      \valuesPtr ->
        glBufferSubData GL_UNIFORM_BUFFER 0 valuesSize (castPtr valuesPtr)

bufferElementData :: MonadIO m => [GLuint] -> m ElementArrayBuffer
bufferElementData values  = do

  buffer <- newElementArrayBuffer

  withElementArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLuint) * length values)

    liftIO . withArray values $ 
      \valuesPtr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER valuesSize (castPtr valuesPtr) GL_STATIC_DRAW

  return buffer

bindElementArrayBuffer :: MonadIO m => ElementArrayBuffer -> m ()
bindElementArrayBuffer elementArrayBuffer = glBindBuffer GL_ELEMENT_ARRAY_BUFFER (unElementArrayBuffer elementArrayBuffer)
