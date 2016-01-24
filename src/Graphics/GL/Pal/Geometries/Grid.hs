{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Grid where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Data.Foldable

gridData :: V3 GLfloat -> V3 Int -> GeometryData
gridData  size subdivisions = GeometryData{..}
  
  where
    subX = subdivisions ^. _x
    subY = subdivisions ^. _y
    subZ = subdivisions ^. _z

    subX' = fromIntegral (subdivisions ^. _x)
    subY' = fromIntegral (subdivisions ^. _y)
    subZ' = fromIntegral (subdivisions ^. _z)

    subXY = (2 * ( subX + 1 ) * (subY + 1))
    subYZ = (2 * ( subY + 1 ) * (subZ + 1))
    subXZ = (2 * ( subX + 1 ) * (subZ + 1))

    sizeX = (size ^. _x)
    sizeY = (size ^. _y)
    sizeZ = (size ^. _z)

    gdNumVerts    = fromIntegral $  (subXY + subYZ + subXZ) 
    gdNumPoints   = fromIntegral gdNumVerts

    gdPositions   = makeGridPositions sizeX sizeY sizeZ subX' subY' subZ'
    gdUVs         = makeGridUVs       $ fromIntegral gdNumVerts
    gdIndices     = makeGridIndicies  $ fromIntegral gdNumVerts 
    gdNormals     = makeGridNormals   $ fromIntegral gdNumVerts
    gdTangents    = makeGridTangents  $ fromIntegral gdNumVerts



makeGridPositions :: GLfloat -> GLfloat -> GLfloat -> Int -> Int -> Int -> [GLfloat]
makeGridPositions xSize ySize zSize subX subY subZ = positions

  where
    
    xVec = V3 1 0 0 :: V3 GLfloat
    yVec = V3 0 1 0 :: V3 GLfloat
    zVec = V3 0 0 1 :: V3 GLfloat


    xSizeVec = realToFrac xSize
    ySizeVec = realToFrac ySize
    zSizeVec = realToFrac zSize

    xSubVec = fromIntegral subX
    ySubVec = fromIntegral subY
    zSubVec = fromIntegral subZ

    

    positions = concat
                [ concat [getPointZ (realToFrac x) (realToFrac y) | x <- [0 .. subX] , y <- [0 .. subY]]
                , concat [getPointX (realToFrac y) (realToFrac z) | y <- [0 .. subY] , z <- [0 .. subZ]]
                , concat [getPointY (realToFrac x) (realToFrac z) | x <- [0 .. subX] , z <- [0 .. subZ]]
                ]

    getPointZ :: V3 GLfloat -> V3 GLfloat -> [GLfloat]
    getPointZ x y = concat [toList pDo , toList pUp]
      where 

        pUp = xVec * (x / xSubVec) * xSizeVec
            + yVec * (y / ySubVec) * ySizeVec
            - 0.5 * xVec * xSizeVec
            - 0.5 * yVec * ySizeVec
            + 0.5 * zVec * zSizeVec

        pDo = xVec * (x / xSubVec) * xSizeVec
            + yVec * (y / ySubVec) * ySizeVec
            - 0.5 * xVec * xSizeVec
            - 0.5 * yVec * ySizeVec
            - 0.5 * zVec * zSizeVec

    getPointY :: V3 GLfloat -> V3 GLfloat -> [GLfloat]
    getPointY x z = concat [toList pDo , toList pUp]
      where 

        pUp = xVec * (x / xSubVec) * xSizeVec
            + zVec * (z / zSubVec) * zSizeVec
            - 0.5 * xVec * xSizeVec
            - 0.5 * zVec * zSizeVec
            + 0.5 * yVec * ySizeVec

        pDo = xVec * (x / xSubVec) * xSizeVec
            + zVec * (z / zSubVec) * zSizeVec
            - 0.5 * xVec * xSizeVec
            - 0.5 * zVec * zSizeVec
            - 0.5 * yVec * ySizeVec

    getPointX :: V3 GLfloat -> V3 GLfloat -> [GLfloat]
    getPointX y z = concat [pDo , pUp]
      where 

        pUp = toList $    yVec * (y / ySubVec) * ySizeVec
                        + zVec * (z / zSubVec) * zSizeVec
                        - 0.5 * yVec * ySizeVec
                        - 0.5 * zVec * zSizeVec
                        + 0.5 * xVec * xSizeVec

        pDo = toList $    yVec * (y / ySubVec) * ySizeVec
                        + zVec * (z / zSubVec) * zSizeVec
                        - 0.5 * yVec * ySizeVec
                        - 0.5 * zVec * zSizeVec
                        - 0.5 * xVec * xSizeVec
        

makeGridIndicies :: GLuint -> [GLuint]
makeGridIndicies numVerts = indices
  where
    indices = [0..numVerts]


makeGridNormals :: GLuint -> [GLfloat]
makeGridNormals numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint _ = toList p
      where
        p = V3 0 1 0

makeGridTangents :: GLuint -> [GLfloat]
makeGridTangents numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint _ = toList p
      where
        p = V3 0 0 1

makeGridUVs :: GLuint -> [GLfloat]
makeGridUVs numVerts = positions
  where
    positions = concatMap getPoint [0..numVerts]
    getPoint _ = toList p
      where
        p = V2 0 0

gridGeometry ::  V3 GLfloat -> V3 Int -> IO Geometry   
gridGeometry size subdivisions = geometryFromData $ gridData size subdivisions 
