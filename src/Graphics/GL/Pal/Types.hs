
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GL.Pal.Types where

import Graphics.GL
import Control.Monad.Trans
import Foreign
import Data.Data
import Debug.Trace

-- | For texture data
data ColorSpace = SRGB | Linear

newtype Program             = Program             { unProgram             :: GLuint } deriving Show

newtype AttributeLocation   = AttributeLocation   { unAttributeLocation   :: GLint  } deriving Show
newtype TextureID           = TextureID           { unTextureID           :: GLuint } deriving Show

newtype VertexArrayObject   = VertexArrayObject   { unVertexArrayObject   :: GLuint } deriving Show
newtype ArrayBuffer         = ArrayBuffer         { unArrayBuffer         :: GLuint } deriving Show
newtype ElementArrayBuffer  = ElementArrayBuffer  { unElementArrayBuffer  :: GLuint } deriving Show
newtype UniformBuffer       = UniformBuffer       { unUniformBuffer       :: GLuint } deriving Show

newtype TextureObject       = TextureObject       { unTextureObject       :: GLuint } deriving Show

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


-- | Utility for extracting a value from a pointer-taking function
overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO (alloca (\p -> f p >> peek p))


traceL :: Show a => String -> a -> a
traceL label value = trace (label ++ ": " ++ show value) value

fI :: ( Integral a , Num b ) => a -> b
fI = fromIntegral
