
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

newtype Program             = Program             { unProgram             :: GLuint }

newtype AttributeLocation   = AttributeLocation   { unAttributeLocation   :: GLint  }
newtype TextureID           = TextureID           { unTextureID           :: GLuint }

newtype VertexArrayObject   = VertexArrayObject   { unVertexArrayObject   :: GLuint }
newtype ArrayBuffer         = ArrayBuffer         { unArrayBuffer         :: GLuint }
newtype ElementArrayBuffer  = ElementArrayBuffer  { unElementArrayBuffer  :: GLuint }

newtype TextureObject       = TextureObject       { unTextureObject       :: GLuint }

-- A UniformLocation is tagged with the type of the uniform it applies to.
newtype UniformLocation a = 
  UniformLocation { unUniformLocation :: GLint }
  deriving (Data, Typeable, Show)

data Geometry = Geometry
  { positions     :: !ArrayBuffer
  , normals       :: !ArrayBuffer
  , tangents      :: !ArrayBuffer
  , uvs           :: !ArrayBuffer
  , indices       :: !ElementArrayBuffer
  , vertCount     :: !GLsizei
  }

data GeometryData = GeometryData
  { positionList  :: ![ GLfloat ]
  , normalList    :: ![ GLfloat ]
  , tangentList   :: ![ GLfloat ]
  , uvList        :: ![ GLfloat ]
  , indexList     :: ![ GLuint  ]
  , numVerts      :: !GLsizei
  , numPoints     :: !GLuint
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
