{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Triangle where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Data.Foldable
import Control.Monad.Trans

triangleData :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> GeometryData
triangleData p1 p2 p3 n1 = GeometryData{..}

  where

    p = 1/sqrt 2
    -- The base Array of vertices
    vertList      = [ p1
                    , p2
                    , p3
                    ]

    faceList      = [ V3 1 2 0 ]

    gdNumVerts  = fromIntegral (length vertList) 

    gdPositions   = makeTrianglePositions size vertList
    gdUVs         = [V2 0 0, V2 0.5 1, V2 1 0]
    gdIndices     = concatMap toList faceList 
    gdNormals     = replicate (length vertList) n1
    gdTangents    = replicate (length vertList) 0

triangleGeometry :: MonadIO m => GLfloat -> m Geometry
triangleGeometry size = geometryFromData $ triangleData size
