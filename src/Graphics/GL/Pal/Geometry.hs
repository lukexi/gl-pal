{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Pal.Geometry where 
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.ArrayBuffer

geometryFromData :: GeometryData -> IO Geometry
geometryFromData GeometryData{..} = do

  geoPositions   <- bufferData         gdPositions
  geoNormals     <- bufferData         gdNormals
  geoTangents    <- bufferData         gdTangents
  geoUVs         <- bufferData         gdUVs
  geoIndices     <- bufferElementData  gdIndices
  
  let geoVertCount = gdNumVerts

  return Geometry{..}
