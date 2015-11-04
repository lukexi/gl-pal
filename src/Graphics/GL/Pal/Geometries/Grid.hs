{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Grid where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
-- import Debug.Trace
-- import Control.Arrow

fI :: ( Integral a , Num b ) => a -> b
fI = fromIntegral

gridData :: V3 GLfloat -> V3 Int -> GeometryData
gridData  size subdivisions = GeometryData{..}

  where
    subX = fromIntegral (subdivisions ^. _x)
    subY = fromIntegral (subdivisions ^. _y)
    subZ = fromIntegral (subdivisions ^. _z)

    subXY = (2 * ( subX + 1 ) * (subY + 1))
    subYZ = (2 * ( subY + 1 ) * (subZ + 1))
    subXZ = (2 * ( subX + 1 ) * (subZ + 1))

    sizeX = fI (size ^. _x)
    sizeY = fI (size ^. _x)
    sizeZ = fI (size ^. _x)

    gdNumVerts    = subXY + subYZ + subXZ
    gdNumPoints   = gdNumVerts

    gdPositions   = makeGridPositions sizeX sizeY sizeZ subX subY subZ
    gdUVs         = makeGridUVs       gdNumVerts
    gdIndices     = makeGridIndicies  gdNumVerts 
    gdNormals     = makeGridNormals   gdNumVerts
    gdTangents    = makeGridTangents  gdNumVerts


makeGridPositions :: GLfloat -> GLfloat -> GLfloat -> Int -> Int -> Int -> [ GLfloat ]
makeGridPositions xSize ySize zSize subX subY subZ = positions

  where
    
    xVec = (V3 1 0 0)::GLfloat
    yVec = (V3 0 1 0)::GLfloat
    zVec = (V3 0 0 1)::GLfloat
    

    positions = concat
                [ concat [ getPointZ x y | x <- [ 0 .. subX ] , y <- [ 0 .. subY ] ]
                , concat [ getPointX y z | y <- [ 0 .. subY ] , z <- [ 0 .. subZ ] ]
                , concat [ getPointY x z | x <- [ 0 .. subX ] , z <- [ 0 .. subZ ] ]
                ]

    getPointZ :: GLfloat -> GLfloat -> [GLfloat]
    getPointZ x y = concat [ (toList pDo) , (toList pUp) ]
      where 

        pUp = xVec * (x / subX) * xSize
            + yVec * (y / subY) * ySize
            - 0.5 * xVec * xSize
            - 0.5 * yVec * ySize
            + 0.5 * zVec * zSize

        pDo = xVec * (x / subX) * xSize
            + yVec * (y / subY) * ySize
            - 0.5 * xVec * xSize
            - 0.5 * yVec * ySize
            - 0.5 * zVec * zSize

    getPointY :: GLfloat -> GLfloat -> [GLfloat]
    getPointY x z = concat [ (toList pDo) , (toList pUp) ]
      where 

        pUp = xVec * (x / fI subX) * xSize
            + zVec * (z / fI subZ) * zSize
            - 0.5 * xVec * xSize
            - 0.5 * zVec * zSize
            + 0.5 * yVec * ySize

        pDo = xVec * (x / fI subX) * xSize
            + zVec * (z / fI subZ) * zSize
            - 0.5 * xVec * xSize
            - 0.5 * zVec * zSize
            - 0.5 * yVec * ySize

    getPointX :: GLfloat -> GLfloat -> [GLfloat]
    getPointX y z = concat [ pDo , pUp ]
      where 

        pUp = toList $    yVec * (y / fI subY) * ySize
                        + zVec * (z / fI subZ) * zSize
                        - 0.5 * yVec * ySize
                        - 0.5 * zVec * zSize
                        + 0.5 * xVec * xSize

        pDo = toList $    yVec * (y / (fI subY)) * (ySize)
                        + zVec * (z / (fI subZ)) * (zSize)
                        - 0.5 * yVec * (ySize)
                        - 0.5 * zVec * (zSize)
                        - 0.5 * xVec * (xSize)
        

makeGridIndicies :: GLuint -> [ GLuint ]
makeGridIndicies numVerts = indices
  where
    indices = [0..numVerts]


makeGridNormals :: GLuint -> [ GLfloat ]
makeGridNormals numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint _ = toList p
      where
        p = V3 0 1 0

makeGridTangents :: GLuint -> [ GLfloat ]
makeGridTangents numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint _ = toList p
      where
        p = V3 0 0 1

makeGridUVs :: GLuint -> [ GLfloat ]
makeGridUVs numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint i = toList p
      where
        p = V2 0 0

gridGeometry ::  V3 GLfloat -> V3 Int -> IO Geometry   
gridGeometry size subdivisions = geometryFromData $ gridData size subdivisions 
