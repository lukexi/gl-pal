{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Line where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace)
import Data.Foldable
import Control.Monad.Trans

lineData :: GLuint -> GeometryData
lineData  subdivisions = GeometryData{..}

  where

    gdNumVerts    = 2 * (( fromIntegral subdivisions ) - 1 )
    gdNumPoints   = fromIntegral subdivisions

    gdPositions   = makeLinePositions subdivisions
    gdUVs         = makeLineUVs       subdivisions
    gdIndices     = makeLineIndicies  subdivisions 
    gdNormals     = makeLineNormals   subdivisions
    gdTangents    = makeLineTangents  subdivisions


makeLinePositions :: GLuint -> [V3 GLfloat]
makeLinePositions subdivisions = positions
  where
    positions = map getPoint [0..subdivisions]
    getPoint i = p
      where
        p = V3 ( fromIntegral i / fromIntegral subdivisions ) 0 0

makeLineIndicies :: GLuint -> [GLuint]
makeLineIndicies subdivisions = indices
  where
    indices = concatMap getPoint [0..(subdivisions-1)]
    getPoint i = toList p
      where
        p = V2 i (i+1)

makeLineNormals :: GLuint -> [V3 GLfloat]
makeLineNormals subdivisions = positions
  where
    positions = map getPoint [0..subdivisions]
    getPoint _ = p
      where
        p = V3 0 1 0

makeLineTangents :: GLuint -> [V3 GLfloat]
makeLineTangents subdivisions = positions
  where
    positions = map getPoint [0..subdivisions]
    getPoint _ = p
      where
        p = V3 0 0 1

makeLineUVs :: GLuint -> [V2 GLfloat]
makeLineUVs subdivisions = positions
  where
    positions = map getPoint [0..subdivisions]
    getPoint i = p
      where
        p = V2 ( fromIntegral i / fromIntegral subdivisions ) 0

lineGeometry :: MonadIO m => GLuint -> m Geometry   
lineGeometry subdivisions = geometryFromData $ lineData subdivisions 


