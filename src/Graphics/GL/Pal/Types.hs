
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GL.Pal.Types where

import Graphics.GL
import Data.Data

-- | For texture data
data ColorSpace = SRGB | Linear

newtype Program             = Program             { unProgram             :: GLuint } deriving Show

newtype AttributeLocation   = AttributeLocation   { unAttributeLocation   :: GLint  } deriving Show
newtype TextureID           = TextureID           { unTextureID           :: GLuint } deriving Show

newtype VertexArrayObject   = VertexArrayObject   { unVertexArrayObject   :: GLuint } deriving Show
newtype ArrayBuffer         = ArrayBuffer         { unArrayBuffer         :: GLuint } deriving Show
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
  { geoPositions     :: !ArrayBuffer
  , geoNormals       :: !ArrayBuffer
  , geoTangents      :: !ArrayBuffer
  , geoUVs           :: !ArrayBuffer
  , geoIndices       :: !ElementArrayBuffer
  , geoVertCount     :: !GLsizei
  }

data GeometryData = GeometryData
  { gdPositions  :: ![ GLfloat ]
  , gdNormals    :: ![ GLfloat ]
  , gdTangents   :: ![ GLfloat ]
  , gdUVs        :: ![ GLfloat ]
  , gdIndices    :: ![ GLuint  ]
  , gdNumVerts   :: !GLsizei
  , gdNumPoints  :: !GLuint
  }

data Shape u = Shape
  { sProgram   :: !Program
  , sUniforms  :: !u
  , sGeometry  :: !Geometry
  , sVAO       :: !VertexArrayObject
  }



