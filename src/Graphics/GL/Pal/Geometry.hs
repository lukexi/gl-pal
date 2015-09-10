{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Pal.Geometry where 
import Graphics.GL.Pal.Types
import Graphics.GL.Pal.ArrayBuffer

geometryFromData :: GeometryData -> IO Geometry
geometryFromData GeometryData{..} = do

  positions   <- bufferData         positionList
  normals     <- bufferData         normalList
  tangents    <- bufferData         tangentList
  uvs         <- bufferData         uvList
  indices     <- bufferElementData  indexList
  
  let vertCount = numVerts

  return Geometry{..}
