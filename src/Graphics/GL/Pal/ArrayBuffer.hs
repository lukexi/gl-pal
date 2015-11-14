module Graphics.GL.Pal.ArrayBuffer where

import Foreign
import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.WithActions

import Control.Monad.Trans

newUniformBuffer :: MonadIO m => m UniformBuffer
newUniformBuffer = UniformBuffer <$> overPtr (glGenBuffers 1)

newArrayBuffer :: MonadIO m => m ArrayBuffer
newArrayBuffer = ArrayBuffer <$> overPtr (glGenBuffers 1)

newElementArrayBuffer :: MonadIO m => m ElementArrayBuffer
newElementArrayBuffer = ElementArrayBuffer <$> overPtr (glGenBuffers 1)

-- | Buffers a list of floats using the given draw hint, e.g. GL_STATIC_DRAW
-- and returns a reference to the ArrayBuffer where the data is stored
bufferData :: GLenum -> [GLfloat] -> IO ArrayBuffer
bufferData drawType values = do

  buffer <- newArrayBuffer

  withArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    withArray values $ 
      \valuesPtr ->
        glBufferData GL_ARRAY_BUFFER valuesSize (castPtr valuesPtr) drawType

  return buffer


bufferUniformData :: GLenum -> [GLfloat] -> IO UniformBuffer
bufferUniformData drawType values = do

  buffer <- newUniformBuffer

  withUniformBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    withArray values $ 
      \valuesPtr ->
        glBufferData GL_UNIFORM_BUFFER valuesSize (castPtr valuesPtr) drawType

  return buffer

bufferSubData :: ArrayBuffer -> [GLfloat] -> IO ()
bufferSubData buffer values = do

  withArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    withArray values $ 
      \valuesPtr ->
        glBufferSubData GL_ARRAY_BUFFER 0 valuesSize (castPtr valuesPtr)

bufferUniformSubData :: UniformBuffer -> [GLfloat] -> IO ()
bufferUniformSubData buffer values = do

  withUniformBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length values)

    withArray values $ 
      \valuesPtr ->
        glBufferSubData GL_UNIFORM_BUFFER 0 valuesSize (castPtr valuesPtr)

bufferElementData :: [GLuint] -> IO ElementArrayBuffer
bufferElementData values  = do

  buffer <- newElementArrayBuffer

  withElementArrayBuffer buffer $ do

    let valuesSize = fromIntegral (sizeOf (undefined :: GLuint) * length values)

    withArray values $ 
      \valuesPtr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER valuesSize (castPtr valuesPtr) GL_STATIC_DRAW

  return buffer

