{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Plane where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Utility
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace  )
import Control.Lens hiding (indices)
import Control.Monad.Trans

planeData :: V2 GLfloat -> V3 GLfloat -> V3 GLfloat -> V2 Int -> GeometryData
planeData size normal up subdivisions = GeometryData{..}

  where
    subdivisionsX = subdivisions ^. _x 
    subdivisionsY = subdivisions ^. _y 

    sizeX         = size ^. _x 
    sizeY         = size ^. _y 

    tangent       = normalize $ cross normal up 
    binormal      = normalize $ cross tangent normal
    
    numVerts      = fromIntegral (length gdPositions)
    gdNormals     = replicate numVerts normal
    gdTangents    = replicate numVerts tangent

    gdPositions   = makePlanePositions sizeX sizeY tangent binormal subdivisionsX subdivisionsY
    gdUVs         = makePlaneUVs                                    subdivisionsX subdivisionsY
    gdIndices     = makePlaneIndices subdivisionsX subdivisionsY


planeGeometry :: MonadIO m => V2 GLfloat -> V3 GLfloat -> V3 GLfloat -> V2 Int -> m Geometry
planeGeometry size normal up subdivisions = geometryFromData $ planeData size normal up subdivisions 


makePlanePositions :: GLfloat -> GLfloat -> V3 GLfloat -> V3 GLfloat -> Int -> Int -> [V3 GLfloat]
makePlanePositions xSize ySize xVec yVec subdivisionsX subdivisionsY = positions
  where

    positions = [ getPoint x y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y =  p
      where 
        p = xVec * (fI x / fI subdivisionsX) * (realToFrac xSize)
          + yVec * (fI y / fI subdivisionsY) * (realToFrac ySize)
          - 0.5 * xVec * (realToFrac xSize)
          - 0.5 * yVec * (realToFrac ySize)



makePlaneUVs :: Int -> Int -> [V2 GLfloat]
makePlaneUVs subdivisionsX subdivisionsY = uvs
  where

    uvs = [getPoint x y | x <- [0..subdivisionsX], y <- [0..subdivisionsY]]
    getPoint x y = p
      where 
        p = V2 (fI x  / fI subdivisionsX)  (fI y / fI subdivisionsY)


makePlaneIndices :: Int -> Int -> [GLuint]
makePlaneIndices subdivisionsX subdivisionsY = map fI indices
  where
    indices = concat [getIndices x y | x <- [0..subdivisionsX - 1], y <- [0..subdivisionsY - 1]]
    getIndices x y = [p1, p3, p2, p2, p3, p4]
      where 

        p1 = (x + 0) * (subdivisionsY + 1) + (y + 0)   
        p3 = (x + 0) * (subdivisionsY + 1) + (y + 1)  
        p2 = (x + 1) * (subdivisionsY + 1) + (y + 0) 
        p4 = (x + 1) * (subdivisionsY + 1) + (y + 1) 
