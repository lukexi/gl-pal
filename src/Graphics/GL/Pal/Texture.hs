{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module Graphics.GL.Pal.Texture where

import Graphics.GL.Pal.Types

import Control.Monad (when)
import Foreign.Ptr
import Graphics.GL
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic
import Prelude hiding (any, ceiling)
import Control.Monad.Trans
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Vector.Storable as SV

loadTexture :: MonadIO m => FilePath -> ColorSpace -> m TextureID
loadTexture path colorSpace = liftIO (JP.readImage path) >>= \case
    Right dimg -> createTexture colorSpace dimg
    Left e -> error e

createTexture :: MonadIO m => ColorSpace -> JP.DynamicImage -> m TextureID
createTexture colorSpace dimg  = do
    t <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D t
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glPixelStorei GL_UNPACK_LSB_FIRST 0
    glPixelStorei GL_UNPACK_SWAP_BYTES 0
    glPixelStorei GL_UNPACK_ROW_LENGTH 0
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_SKIP_ROWS 0
    glPixelStorei GL_UNPACK_SKIP_PIXELS 0
    glPixelStorei GL_UNPACK_SKIP_IMAGES 0
    glPixelStorei GL_UNPACK_ALIGNMENT 1

    let (width, height) = imageDims dimg
    glTexStorage2D
       GL_TEXTURE_2D
       (floor (logBase (2 :: Float)
                       (fromIntegral (max width height))))
       (case colorSpace of
          SRGB -> GL_SRGB8
          Linear -> GL_RGB32F)
       width
       height

    updateTexture (TextureID t) dimg

    when gl_EXT_texture_filter_anisotropic
        (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 16)

    return (TextureID t)

imageDims :: (Num t) => JP.DynamicImage -> (t, t)
imageDims dimg =
    let width  = fromIntegral (JP.dynamicMap JP.imageWidth dimg)
        height = fromIntegral (JP.dynamicMap JP.imageHeight dimg)
    in (width, height)

updateTexture :: MonadIO m => TextureID -> JP.DynamicImage -> m ()
updateTexture (TextureID t) dimg = do
    glBindTexture GL_TEXTURE_2D t
    let (width, height) = imageDims dimg
    case dimg of
        JP.ImageRGB8 (JP.Image _ _ d) ->
            liftIO $ SV.unsafeWith d
              (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height GL_RGB GL_UNSIGNED_BYTE .
               castPtr)
        JP.ImageRGBA8 (JP.Image _ _ d) ->
            liftIO $ SV.unsafeWith d
              (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height GL_RGBA GL_UNSIGNED_BYTE .
               castPtr)
        JP.ImageYCbCr8 img ->
            let toRgb8 = JP.convertPixel :: JP.PixelYCbCr8 -> JP.PixelRGB8
            in case JP.pixelMap toRgb8 img of
                 JP.Image _ _ d ->
                   liftIO $ SV.unsafeWith d
                     (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height GL_RGB GL_UNSIGNED_BYTE .
                      castPtr)
        _ -> error "Unknown image format"
    glGenerateMipmap GL_TEXTURE_2D
