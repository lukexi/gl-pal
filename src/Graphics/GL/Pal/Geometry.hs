{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Pal.Geometry where 
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.ArrayBuffer
import Graphics.GL

geometryFromData :: GeometryData -> IO Geometry
geometryFromData GeometryData{..} = do

  geoPositions   <- bufferData GL_STATIC_DRAW gdPositions
  geoNormals     <- bufferData GL_STATIC_DRAW gdNormals
  geoTangents    <- bufferData GL_STATIC_DRAW gdTangents
  geoUVs         <- bufferData GL_STATIC_DRAW gdUVs
  geoIndices     <- bufferElementData gdIndices
  
  let geoVertCount = gdNumVerts

  return Geometry{..}
