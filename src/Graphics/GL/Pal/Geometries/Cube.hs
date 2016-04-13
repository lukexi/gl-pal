{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Cube where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry
import Graphics.GL.Pal.Geometries.Shared
import Graphics.GL.Pal.Geometries.Plane

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Control.Monad.Trans

cubeData :: V3 GLfloat -> V3 Int -> GeometryData
cubeData size subdivisions = mergeGeometries planes
  where 
    n1 = V3 0 0 1 
    u1 = V3 0 1 0
    s1 = size         ^. _xy 
    d1 = subdivisions ^. _xy 

    n2 = V3 1 0 0 
    u2 = V3 0 1 0
    s2 = size         ^. _zy 
    d2 = subdivisions ^. _zy 
    
    n3 = V3 0 0 (-1) 
    u3 = V3 0 1 0
    s3 = size         ^. _xy 
    d3 = subdivisions ^. _xy 
    
    n4 = V3 (-1) 0 0
    u4 = V3 0 1 0
    s4 = size         ^. _zy 
    d4 = subdivisions ^. _zy 
    
    n5 = V3 0 1 0
    u5 = V3 1 0 0
    s5 = size         ^. _zx 
    d5 = subdivisions ^. _zx 
    
    n6 = V3 0 (-1) 0
    u6 = V3 (-1) 0 0
    s6 = size         ^. _zx 
    d6 = subdivisions ^. _zx 
    
    planes = [ shiftPoints (n1 * size / 2) $ planeData s1 n1 u1 d1
             , shiftPoints (n2 * size / 2) $ planeData s2 n2 u2 d2
             , shiftPoints (n3 * size / 2) $ planeData s3 n3 u3 d3
             , shiftPoints (n4 * size / 2) $ planeData s4 n4 u4 d4
             , shiftPoints (n5 * size / 2) $ planeData s5 n5 u5 d5
             , shiftPoints (n6 * size / 2) $ planeData s6 n6 u6 d6
             ]

cubeGeometry :: MonadIO m => V3 GLfloat -> V3 Int -> m Geometry
cubeGeometry size subdivisions = geometryFromData $ cubeData size subdivisions 
