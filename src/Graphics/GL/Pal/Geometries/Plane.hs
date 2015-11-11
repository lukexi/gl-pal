{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Plane where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace  )
import Control.Lens hiding (indices)
import Data.Foldable

-- import Debug.Trace


planeData :: V2 GLfloat -> V3 GLfloat -> V3 GLfloat -> V2 Int -> GeometryData
planeData size normal up subdivisions = GeometryData{..}

  where

    gdNumVerts    = 3 * 2 * (fI subdivisionsX ) * (fI subdivisionsY)
    gdNumPoints   = (fI subdivisionsX + 1) * (fI subdivisionsY + 1)

    subdivisionsX = subdivisions ^. _x 
    subdivisionsY = subdivisions ^. _y 

    sizeX         = size ^. _x 
    sizeY         = size ^. _y 

    tangent       = normalize $ cross normal up 
    binormal      = normalize $ cross tangent normal
    
    gdNormals     = take (fI gdNumPoints * 3) $ cycle [ normal  ^. _x , normal  ^. _y , normal  ^. _z ]
    gdTangents    = take (fI gdNumPoints * 3) $ cycle [ tangent ^. _x , tangent ^. _y , tangent ^. _z ]

    gdPositions   = makePlanePositions sizeX sizeY tangent binormal subdivisionsX subdivisionsY
    gdUVs         = makePlaneUVs                                    subdivisionsX subdivisionsY
    gdIndices     = makePlaneIndices subdivisionsX subdivisionsY


planeGeometry :: V2 GLfloat -> V3 GLfloat -> V3 GLfloat -> V2 Int -> IO Geometry
planeGeometry size normal up subdivisions = geometryFromData $ planeData size normal up subdivisions 


makePlanePositions :: GLfloat -> GLfloat -> V3 GLfloat -> V3 GLfloat -> Int -> Int -> [GLfloat]
makePlanePositions xSize ySize xVec yVec subdivisionsX subdivisionsY = positions
  where

    positions = concat [ getPoint x y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y =  toList p
      where 
        p = xVec * (fI x / fI subdivisionsX) * (realToFrac xSize)
          + yVec * (fI y / fI subdivisionsY) * (realToFrac ySize)
          - 0.5 * xVec * (realToFrac xSize)
          - 0.5 * yVec * (realToFrac ySize)



makePlaneUVs :: Int -> Int -> [GLfloat]
makePlaneUVs subdivisionsX subdivisionsY = uvs
  where

    uvs = concat [ getPoint x  y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y = toList p
      where 
        p = V2 (fI x  / fI subdivisionsX)  (fI y / fI subdivisionsY)


makePlaneIndices :: Int -> Int -> [GLuint]
makePlaneIndices subdivisionsX subdivisionsY = map fI indices
  where
    indices = concat [ getIndices x  y | x <- [ 0 .. subdivisionsX - 1 ] , y <- [ 0 .. subdivisionsY - 1 ] ]
    getIndices x y = [ p1 , p3 , p2 , p2 , p3 , p4 ]
      where 

        p1 = (x + 0) * (subdivisionsY + 1) + (y + 0)   
        p3 = (x + 0) * (subdivisionsY + 1) + (y + 1)  
        p2 = (x + 1) * (subdivisionsY + 1) + (y + 0) 
        p4 = (x + 1) * (subdivisionsY + 1) + (y + 1) 
