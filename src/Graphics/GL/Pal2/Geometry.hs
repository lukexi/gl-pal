{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Pal2.Geometry where 
import Graphics.GL.Pal2.Types
import Graphics.GL.Pal2.ArrayBuffer

import Debug.Trace

geometryFromShape :: Shape -> IO Geometry
geometryFromShape Shape{..} = do

  positions   <- bufferData         positionList
  normals     <- bufferData         normalList
  tangents    <- bufferData         tangentList
  uvs         <- bufferData         uvList
  indices     <- bufferElementData  indexList
  
  let vertCount = traceShowId $ numVerts

  return Geometry{..}