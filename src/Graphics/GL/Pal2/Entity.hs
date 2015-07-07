{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal2.Entity where
  
import Graphics.GL

import Graphics.GL.Pal2.Types
import Graphics.GL.Pal2.Uniforms
import Graphics.GL.Pal2.WithActions
import Graphics.GL.Pal2.AssignAttribute
import Data.Data

entity :: (Data u) => Geometry -> Program -> IO (Entity u)
entity geometry program = do

  -- Setup a VAO
  vAO <- VertexArrayObject <$> overPtr ( glGenVertexArrays 1 )

  withVAO vAO $ do

    withArrayBuffer ( positions   geometry ) $ assignAttribute program "aPosition" 3 
    withArrayBuffer ( normals     geometry ) $ assignAttribute program "aNormal"   3 
    withArrayBuffer ( tangents    geometry ) $ assignAttribute program "aTangent"  3
    withArrayBuffer ( uvs         geometry ) $ assignAttribute program "aUV"       2 

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ( unElementArrayBuffer ( indices geometry ))


  uniforms <- acquireUniforms program

  return Entity{..}


  

