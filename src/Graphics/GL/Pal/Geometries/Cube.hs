{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Cube where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry
import Graphics.GL.Pal.Geometries.Plane

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Control.Monad.Trans

cubeData :: V3 GLfloat -> V3 Int -> GeometryData
cubeData size subdivisions = finalData
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
    
    plane1 = planeData s1 n1 u1 d1
    plane2 = planeData s2 n2 u2 d2
    plane3 = planeData s3 n3 u3 d3
    plane4 = planeData s4 n4 u4 d4
    plane5 = planeData s5 n5 u5 d5
    plane6 = planeData s6 n6 u6 d6

    planeNumPoints = map gdNumPoints [plane1, plane2, plane3, plane4, plane5, plane6]

    f1 = shiftPlane plane1 n1 (sum . take 0 $ planeNumPoints)
    f2 = shiftPlane plane2 n2 (sum . take 1 $ planeNumPoints)
    f3 = shiftPlane plane3 n3 (sum . take 2 $ planeNumPoints)
    f4 = shiftPlane plane4 n4 (sum . take 3 $ planeNumPoints)
    f5 = shiftPlane plane5 n5 (sum . take 4 $ planeNumPoints)
    f6 = shiftPlane plane6 n6 (sum . take 5 $ planeNumPoints)
    fs = [f1,f2,f3,f4,f5,f6]

    finalData = GeometryData 
        { gdPositions = concatMap gdPositions fs
        , gdIndices   = concatMap gdIndices   fs
        , gdUVs       = concatMap gdUVs       fs
        , gdNormals   = concatMap gdNormals   fs
        , gdTangents  = concatMap gdTangents  fs
        , gdNumVerts  = sum $ map gdNumVerts  fs
        , gdNumPoints = sum $ map gdNumPoints fs
        }

    shiftPlane :: GeometryData -> V3 GLfloat -> GLuint -> GeometryData
    shiftPlane plane normal startIndex = plane { gdPositions = newPositions, gdIndices = newIndices }
      where
        newPositions = map (+ (normal * size / 2)) (gdPositions plane)
        newIndices   = map (+ startIndex)          (gdIndices plane)


cubeGeometry :: MonadIO m => V3 GLfloat -> V3 Int -> m Geometry
cubeGeometry size subdivisions = geometryFromData $ cubeData size subdivisions 
