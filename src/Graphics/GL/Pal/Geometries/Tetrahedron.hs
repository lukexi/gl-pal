{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Tetrahedron where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Data.Foldable
import Control.Monad.Trans

v3 :: Num a => (a, a, a) -> V3 a
v3 (x,y,z) = V3 x y z 

tetrahedronData :: GLfloat -> GeometryData
tetrahedronData size = GeometryData{..}

  where

    p = 1/sqrt 2
    -- The base Array of vertices
    vertList      = [ v3 (-1,  0, -p) -- < 0
                    , v3 ( 1,  0, -p) -- > 1
                    , v3 ( 0,  1,  p) -- ^ 2
                    , v3 ( 0, -1,  p) -- v 3
                    ]

    faceList      = [ V3 1 2 0 -- front
                    , V3 1 3 0 -- bottom
                    , V3 1 2 3 -- right
                    , V3 2 0 3 -- left
                    ]
    
    gdPositions   = makeTetrahedronPositions size vertList
    gdUVs         = makeTetrahedronUVs            vertList
    gdIndices     = makeTetrahedronIndices        faceList 
    gdNormals     = makeTetrahedronNormals        vertList
    gdTangents    = makeTetrahedronTangents       vertList


makeTetrahedronPositions :: GLfloat -> [V3 GLfloat] -> [V3 GLfloat]
makeTetrahedronPositions size vertList  = map (realToFrac size *) vertList

makeTetrahedronIndices :: (Foldable t, Foldable t1) => t (t1 b) -> [b]
makeTetrahedronIndices indexList        = concatMap toList indexList

makeTetrahedronNormals :: [V3 GLfloat] -> [V3 GLfloat]
makeTetrahedronNormals positionList     = map normalize positionList

makeTetrahedronUVs :: [V3 GLfloat] -> [V2 GLfloat]
makeTetrahedronUVs positionList = uvs
    where 
        uvs = map getUV positionList
        getUV p = 
            let u = asin (p^._x)/pi + 0.5
                v = asin (p^._y)/pi + 0.5
            in V2 u (1 - v)

makeTetrahedronTangents :: [V3 GLfloat] -> [V3 GLfloat]
makeTetrahedronTangents positionList = replicate (length positionList) 0

tetrahedronGeometry :: MonadIO m => GLfloat -> m Geometry
tetrahedronGeometry size = geometryFromData $ tetrahedronData size
