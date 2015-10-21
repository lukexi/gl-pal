{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Line where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
import Debug.Trace
import Control.Arrow

--fI :: ( Integral a , Num b ) => a -> b
--fI = fromIntegral

lineData :: GLuint -> GeometryData
lineData  subdivisions = GeometryData{..}

  where

    numVerts  = 2 * (( fromIntegral subdivisions ) - 1 )
    numPoints = fromIntegral subdivisions

    posArray      = makeLinePositions subdivisions
    uvArray       = makeLineUVs       subdivisions
    indexArray    = makeLineIndicies  subdivisions 
    normalArray   = makeLineNormals   subdivisions
    tanArray      = makeLineTangents  subdivisions



    positionList  = posArray
    normalList    = normalArray
    
    tangentList   = tanArray
    uvList        = uvArray

    indexList     = indexArray  


makeLinePositions :: GLuint -> [ GLfloat ]
makeLinePositions subdivisions = positions
  where
    positions = concatMap getPoint [0..subdivisions]
    getPoint i = toList p
      where
        p = V3 ( fromIntegral i / fromIntegral subdivisions ) 0 0

makeLineIndicies :: GLuint -> [ GLuint ]
makeLineIndicies subdivisions = indices
  where
    indices = concatMap getPoint [0..(subdivisions-1)]
    getPoint i = toList p
      where
        p = V2 i (i+1)

makeLineNormals :: GLuint -> [ GLfloat ]
makeLineNormals subdivisions = positions
  where
    positions = concatMap getPoint [0..subdivisions]
    getPoint i = toList p
      where
        p = V3 0 1 0

makeLineTangents :: GLuint -> [ GLfloat ]
makeLineTangents subdivisions = positions
  where
    positions = concatMap getPoint [0..subdivisions]
    getPoint i = toList p
      where
        p = V3 0 0 1

makeLineUVs :: GLuint -> [ GLfloat ]
makeLineUVs subdivisions = positions
  where
    positions = concatMap getPoint [0..subdivisions]
    getPoint i = toList p
      where
        p = V2 ( fromIntegral i / fromIntegral subdivisions ) 0

lineGeometry :: GLuint -> IO Geometry   
lineGeometry subdivisions = geometryFromData $ lineData subdivisions 


    

