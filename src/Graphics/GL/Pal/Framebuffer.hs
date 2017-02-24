{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Pal.Framebuffer where

import Graphics.GL
import Control.Monad.Trans
import Graphics.GL.Pal.Types
import Control.Monad
import Foreign
import Baseline

bindFramebuffer :: MonadIO m => Framebuffer -> m ()
bindFramebuffer (Framebuffer framebuffer) = glBindFramebuffer GL_FRAMEBUFFER framebuffer

withFramebuffer :: MonadIO m => Framebuffer -> m a -> m ()
withFramebuffer (Framebuffer framebuffer) action = do
    glBindFramebuffer GL_FRAMEBUFFER framebuffer
    _ <- action
    glBindFramebuffer GL_FRAMEBUFFER 0


-- | Create and configure the texture to use for our framebuffer
createFramebufferTexture :: MonadIO m => GLenum -> GLsizei -> GLsizei -> m TextureID
createFramebufferTexture storage sizeX sizeY = do
    texID <- overPtr (glGenTextures 1)

    glBindTexture   GL_TEXTURE_2D texID
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
    glTexStorage2D  GL_TEXTURE_2D 1 storage sizeX sizeY
    glBindTexture   GL_TEXTURE_2D 0

    return (TextureID texID)

newFramebuffer :: MonadIO m => m Framebuffer
newFramebuffer = Framebuffer <$> overPtr (glGenFramebuffers 1)

-- Create a flat render target with the given size and storage format (and no depth or stencil buffer)
createRenderTexture :: MonadIO m => GLenum -> GLsizei -> GLsizei -> m (Framebuffer, TextureID)
createRenderTexture storage sizeX sizeY = do
    TextureID framebufferTextureID <- createFramebufferTexture storage sizeX sizeY

    framebuffer <- newFramebuffer
    withFramebuffer framebuffer $ do
        -- Attach the texture as the color buffer
        glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D framebufferTextureID 0

        -- Clear the texture
        glClearColor 0 0 0 0
        glClear GL_COLOR_BUFFER_BIT

    return (framebuffer, TextureID framebufferTextureID)

-- | Create an RGBA8 framebuffer with 32-bit depth and 8-bit stencil buffer, suitable for 3D render-to-texture
createFramebuffer :: MonadIO m => GLsizei -> GLsizei -> m (Framebuffer, TextureID)
createFramebuffer sizeX sizeY = do
    TextureID framebufferTextureID <- createFramebufferTexture GL_RGBA8 sizeX sizeY

    framebuffer <- newFramebuffer

    -- Attach the eye texture as the color buffer
    withFramebuffer framebuffer $ do

        glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D framebufferTextureID 0

        -- Generate a render buffer for depth
        renderbuffer <- overPtr (glGenRenderbuffers 1)

        -- Configure the depth buffer dimensions to match the eye texture
        glBindRenderbuffer GL_RENDERBUFFER renderbuffer
        glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH32F_STENCIL8 sizeX sizeY
        glBindRenderbuffer GL_RENDERBUFFER 0

        -- Attach the render buffer as the depth target
        glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_RENDERBUFFER renderbuffer

    return (framebuffer, TextureID framebufferTextureID)

data MultisampleFramebuffer = MultisampleFramebuffer
    { mfbRenderFramebufferID :: Framebuffer
    , mfbRenderTextureID :: TextureID
    , mfbResolveFramebufferID :: Framebuffer
    , mfbResolveTextureID :: TextureID
    , mfbWidth  :: GLint
    , mfbHeight :: GLint
    }

data MSAASamples = MSAASamples1
                 | MSAASamples2
                 | MSAASamples4
                 | MSAASamples8
                 | MSAASamples16

msaaSamplesToNum :: MSAASamples -> GLsizei
msaaSamplesToNum MSAASamples1 = 1
msaaSamplesToNum MSAASamples2 = 2
msaaSamplesToNum MSAASamples4 = 4
msaaSamplesToNum MSAASamples8 = 8
msaaSamplesToNum MSAASamples16 = 16

createMultisampleFramebuffer :: MonadIO m => MSAASamples -> GLsizei -> GLsizei -> m MultisampleFramebuffer
createMultisampleFramebuffer msaaSamples sizeX sizeY = do
    let numSamples = msaaSamplesToNum msaaSamples

    renderFramebufferID <- overPtr (glGenFramebuffers 1)
    glBindFramebuffer GL_FRAMEBUFFER renderFramebufferID

    depthBufferID <- overPtr (glGenRenderbuffers 1)
    glBindRenderbuffer GL_RENDERBUFFER depthBufferID

    glRenderbufferStorageMultisample GL_RENDERBUFFER numSamples GL_DEPTH32F_STENCIL8 sizeX sizeY
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_RENDERBUFFER depthBufferID

    renderTextureID <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D_MULTISAMPLE renderTextureID
    glTexImage2DMultisample GL_TEXTURE_2D_MULTISAMPLE numSamples GL_RGBA8 sizeX sizeY GL_TRUE
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D_MULTISAMPLE renderTextureID 0

    resolveFramebufferID <- overPtr (glGenFramebuffers 1)
    glBindFramebuffer GL_FRAMEBUFFER resolveFramebufferID

    resolveTextureID <- overPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D resolveTextureID
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
    glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 sizeX sizeY 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D resolveTextureID 0

    -- check FBO status
    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    when (status /= GL_FRAMEBUFFER_COMPLETE) $
        error "createMultisampleFramebuffer: Framebuffer status incomplete"

    glBindFramebuffer GL_FRAMEBUFFER 0

    return MultisampleFramebuffer
        { mfbRenderFramebufferID = Framebuffer renderFramebufferID
        , mfbRenderTextureID = TextureID renderTextureID
        , mfbResolveFramebufferID = Framebuffer resolveFramebufferID
        , mfbResolveTextureID = TextureID resolveTextureID
        , mfbWidth = sizeX
        , mfbHeight = sizeY
        }

withMultisamplingFramebuffer :: MonadIO m => MultisampleFramebuffer -> m a -> m ()
withMultisamplingFramebuffer MultisampleFramebuffer{..} action = do

    glEnable GL_MULTISAMPLE

    glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer mfbRenderFramebufferID)

    _ <- action

    glBindFramebuffer GL_FRAMEBUFFER 0

    glDisable GL_MULTISAMPLE

    glBindFramebuffer GL_READ_FRAMEBUFFER (unFramebuffer mfbRenderFramebufferID)
    glBindFramebuffer GL_DRAW_FRAMEBUFFER (unFramebuffer mfbResolveFramebufferID)

    glBlitFramebuffer 0 0 mfbWidth mfbHeight 0 0 mfbWidth mfbHeight
        GL_COLOR_BUFFER_BIT
        GL_LINEAR

    glBindFramebuffer GL_READ_FRAMEBUFFER 0
    glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
