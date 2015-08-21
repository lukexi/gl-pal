{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal2.Geometries.Cube where

import Graphics.GL

import Graphics.GL.Pal2.Types
import Graphics.GL.Pal2.Geometry
import Graphics.GL.Pal2.Geometries.Plane

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
import Debug.Trace

cubeShape :: V3 GLfloat -> V3 Int -> Shape
cubeShape size subdivisions =

  let n1 = V3 0 0 1 
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
      


  in makeCubePoints ( n1 , n2 , n3 , n4 , n5 , n6 )
                    ( u1 , u2 , u3 , u4 , u5 , u6 )
                    ( s1 , s2 , s3 , s4 , s5 , s6 )
                    ( d1 , d2 , d3 , d4 , d5 , d6 )
                    size

-- Cool type signature holmes
makeCubePoints :: (V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat)
               -> (V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat,
                   V3 GLfloat)
               -> (V2 GLfloat,
                   V2 GLfloat,
                   V2 GLfloat,
                   V2 GLfloat,
                   V2 GLfloat,
                   V2 GLfloat)
               -> (V2 Int, V2 Int, V2 Int, V2 Int, V2 Int, V2 Int)
               -> V3 GLfloat
               -> Shape
makeCubePoints (n1,n2,n3,n4,n5,n6) (u1,u2,u3,u4,u5,u6) (s1,s2,s3,s4,s5,s6) (d1,d2,d3,d4,d5,d6) size = finalShape
  where 

    plane1 = planeShape s1 n1 u1 d1
    plane2 = planeShape s2 n2 u2 d2
    plane3 = planeShape s3 n3 u3 d3
    plane4 = planeShape s4 n4 u4 d4
    plane5 = planeShape s5 n5 u5 d5
    plane6 = planeShape s6 n6 u6 d6
    planes = [plane1, plane2, plane3, plane4, plane5, plane6]
    planeNumPoints = map numPoints planes

    f1 = updatePlanePos plane1 n1 (numPoints plane1) (sum . take 0 $ planeNumPoints)
    f2 = updatePlanePos plane2 n2 (numPoints plane2) (sum . take 1 $ planeNumPoints)
    f3 = updatePlanePos plane3 n3 (numPoints plane3) (sum . take 2 $ planeNumPoints)
    f4 = updatePlanePos plane4 n4 (numPoints plane4) (sum . take 3 $ planeNumPoints)
    f5 = updatePlanePos plane5 n5 (numPoints plane5) (sum . take 4 $ planeNumPoints)
    f6 = updatePlanePos plane6 n6 (numPoints plane6) (sum . take 5 $ planeNumPoints)
    fs = [f1,f2,f3,f4,f5,f6]

    finalShape = Shape 
      { positionList = concatMap positionList fs
      , indexList    = concatMap indexList fs
      , uvList       = concatMap uvList fs
      , normalList   = concatMap normalList fs
      , tangentList  = concatMap tangentList fs
      , numVerts     = sum $ map numVerts fs
      , numPoints    = sum $ map numPoints fs
      } 

    updatePlanePos :: Shape -> V3 GLfloat -> GLuint -> GLuint -> Shape
    updatePlanePos plane normal nPoints startIndex = plane { positionList = fPos, indexList = fIndex }
      where
        pos    = positionList plane
        posX   = take ( fI nPoints * 3  ) $ cycle ( toList ( normal * size * V3 0.5 0.5 0.5 ) )
        fPos   = zipWith (+) pos posX
        newI   = startIndex + 0
        fIndex = map ( + newI ) ( indexList plane )

traceL :: Show a => String -> a -> a
traceL label value = trace (label ++ ": " ++ show value) value
      
cubeGeometry :: V3 GLfloat -> V3 Int -> IO Geometry   
cubeGeometry size subdivisions = geometryFromShape $ cubeShape size subdivisions 
