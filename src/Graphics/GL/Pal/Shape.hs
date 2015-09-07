{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Shape where
  
import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.InferUniforms
import Graphics.GL.Pal.WithActions
import Graphics.GL.Pal.AssignAttribute
import Data.Data

-- | A shape is the combination of a VAO, a program, 
-- and the collection of uniforms for that program.
makeShape :: Data u => Geometry -> Program -> IO (Shape u)
makeShape sGeometry sProgram = do

  -- Setup a VAO
  sVAO <- VertexArrayObject <$> overPtr ( glGenVertexArrays 1 )

  withVAO sVAO $ do

    withArrayBuffer ( positions   sGeometry ) $ assignAttribute sProgram "aPosition" 3 
    withArrayBuffer ( normals     sGeometry ) $ assignAttribute sProgram "aNormal"   3 
    withArrayBuffer ( tangents    sGeometry ) $ assignAttribute sProgram "aTangent"  3
    withArrayBuffer ( uvs         sGeometry ) $ assignAttribute sProgram "aUV"       2 

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ( unElementArrayBuffer ( indices sGeometry ))


  sUniforms <- acquireUniforms sProgram

  return Shape{..}


  

