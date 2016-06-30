{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GL.Pal.ArrayBuffer where

import Foreign
import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Utility
import Graphics.GL.Pal.WithActions

import Control.Monad.Trans
import qualified Data.Vector.Storable.Mutable as V
import Data.Vector.Storable.Mutable (IOVector)
import Data.Proxy

genBuffer :: MonadIO m => m GLuint
genBuffer = overPtr (glGenBuffers 1)

newUniformBuffer :: MonadIO m => m UniformBuffer
newUniformBuffer = UniformBuffer <$> genBuffer

bindUniformBufferBase :: MonadIO m => UniformBuffer -> UniformBlockBindingPoint -> m ()
bindUniformBufferBase buffer bindingPoint =
    glBindBufferBase GL_UNIFORM_BUFFER
        (unUniformBlockBindingPoint bindingPoint)
        (unUniformBuffer buffer)


newArrayBuffer :: MonadIO m => m (ArrayBuffer a)
newArrayBuffer = ArrayBuffer <$> genBuffer

newElementArrayBuffer :: MonadIO m => m ElementArrayBuffer
newElementArrayBuffer = ElementArrayBuffer <$> genBuffer

-- | Buffers a list of floats using the given draw hint, e.g. GL_STATIC_DRAW
-- and returns a reference to the ArrayBuffer where the data is stored
bufferData :: forall a m. (Storable a, MonadIO m) => GLenum -> [a] -> m (ArrayBuffer a)
bufferData drawType values = do

    buffer <- newArrayBuffer

    withArrayBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * length values)

        liftIO . withArray values $
            \valuesPtr ->
                glBufferData GL_ARRAY_BUFFER valuesSize (castPtr valuesPtr) drawType

    return buffer

bufferDataV :: forall a m. (Storable a, MonadIO m) => GLenum -> IOVector a -> m (ArrayBuffer a)
bufferDataV drawType values = do

    buffer <- newArrayBuffer

    withArrayBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * V.length values)

        liftIO . V.unsafeWith values $
            \valuesPtr ->
                glBufferData GL_ARRAY_BUFFER valuesSize (castPtr valuesPtr) drawType

    return buffer

bufferDataEmpty :: forall a m. (Storable a, MonadIO m) => GLenum -> Int -> Proxy a -> m (ArrayBuffer a)
bufferDataEmpty drawType numItems _proxy = do

    buffer <- newArrayBuffer

    withArrayBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * numItems)

        glBufferData GL_ARRAY_BUFFER valuesSize nullPtr drawType

    return buffer

bufferSubDataV :: forall a m. (Storable a, MonadIO m) => (ArrayBuffer a) -> IOVector a -> m ()
bufferSubDataV buffer values = do

    withArrayBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * V.length values)

        liftIO . V.unsafeWith values $
            \valuesPtr ->
                glBufferSubData GL_ARRAY_BUFFER 0 valuesSize (castPtr valuesPtr)



bufferUniformData :: forall a m. (Storable a, MonadIO m) => GLenum -> [a] -> m UniformBuffer
bufferUniformData drawType values = do

    buffer <- newUniformBuffer

    withUniformBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * length values)

        liftIO . withArray values $
            \valuesPtr ->
                glBufferData GL_UNIFORM_BUFFER valuesSize (castPtr valuesPtr) drawType

    return buffer

bufferSubData :: forall a m. (Storable a, MonadIO m) => (ArrayBuffer a) -> [a] -> m ()
bufferSubData buffer values = do

    withArrayBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * length values)

        liftIO . withArray values $
            \valuesPtr ->
                glBufferSubData GL_ARRAY_BUFFER 0 valuesSize (castPtr valuesPtr)

bufferUniformSubData :: forall a m. (Storable a, MonadIO m) => UniformBuffer -> [a] -> m ()
bufferUniformSubData buffer values = do

    withUniformBuffer buffer $ do

        let valuesSize = fromIntegral (sizeOf (undefined :: a) * length values)

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
