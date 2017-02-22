
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GL.Pal.Types where

import Graphics.GL
import Linear.Extra
import Data.Data

-- | For texture data
data ColorSpace = SRGB | Linear

newtype Program             = Program             { unProgram             :: GLuint } deriving Show
newtype Shader              = Shader              { unShader              :: GLuint } deriving Show

newtype AttributeLocation   = AttributeLocation   { unAttributeLocation   :: GLint  } deriving Show
newtype TextureID           = TextureID           { unTextureID           :: GLuint }
  deriving (Eq, Show, Ord, Data)

newtype VertexArrayObject   = VertexArrayObject   { unVertexArrayObject   :: GLuint } deriving Show
newtype ArrayBuffer a       = ArrayBuffer         { unArrayBuffer         :: GLuint } deriving Show
newtype ElementArrayBuffer  = ElementArrayBuffer  { unElementArrayBuffer  :: GLuint } deriving Show
newtype UniformBuffer       = UniformBuffer       { unUniformBuffer       :: GLuint } deriving Show

newtype Framebuffer         = Framebuffer         { unFramebuffer         :: GLuint } deriving Show
newtype Renderbuffer        = Renderbuffer        { unRenderbuffer        :: GLuint } deriving Show

newtype UniformBlockBindingPoint = UniformBlockBindingPoint { unUniformBlockBindingPoint :: GLuint } deriving Show
newtype UniformBlockIndex        = UniformBlockIndex        { unUniformBlockIndex        :: GLuint } deriving Show

-- A UniformLocation is tagged with the type of the uniform it applies to.
newtype UniformLocation a =
  UniformLocation { unUniformLocation :: GLint }
  deriving (Data, Typeable, Show)

data Geometry = Geometry
  { geoPositions     :: !(ArrayBuffer (V3 GLfloat))
  , geoNormals       :: !(ArrayBuffer (V3 GLfloat))
  , geoTangents      :: !(ArrayBuffer (V3 GLfloat))
  , geoUVs           :: !(ArrayBuffer (V2 GLfloat))
  , geoIndices       :: !ElementArrayBuffer
  , geoIndexCount     :: !GLsizei
  }

data GeometryData = GeometryData
  { gdPositions  :: ![ V3 GLfloat ]
  , gdNormals    :: ![ V3 GLfloat ]
  , gdTangents   :: ![ V3 GLfloat ]
  , gdUVs        :: ![ V2 GLfloat ]
  , gdIndices    :: ![ GLuint     ]
  }

data Shape u = Shape
  { sProgram   :: !Program
  , sUniforms  :: !u
  , sGeometry  :: !Geometry
  , sVAO       :: !VertexArrayObject
  }



