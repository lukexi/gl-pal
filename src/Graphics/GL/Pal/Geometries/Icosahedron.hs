{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Icosahedron where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding (trace)
import Control.Lens hiding (indices)
import Data.Foldable
import Control.Arrow
import Control.Monad.Trans

icosahedronData :: GLfloat  -> GLuint -> GeometryData
icosahedronData size subdivisions = GeometryData{..}

  where

    -- The base Array of vertices
    vertList      = [ V3 1 0 0 
                    , V3 0 1 0 
                    , V3 0 0 1 
                    , V3 (-1) 0 0 
                    , V3 0 (-1) 0 
                    , V3 0 0 (-1) 
                    ]

    faceList      = [ V3 2 0 1
                    , V3 0 5 1 
                    , V3 5 3 1
                    , V3 3 2 1
                    , V3 0 2 4
                    , V3 5 0 4
                    , V3 3 5 4
                    , V3 2 3 4
                    ]


    (newVertList , newFaceList) = subdivide (vertList, faceList) subdivisions

    gdNumVerts  = 3 * fromIntegral (length newFaceList) 
    gdNumPoints = 3 * fromIntegral (length newVertList)

    gdPositions   = makeIcosahedronPositions size newVertList
    gdUVs         = makeIcosahedronUVs            newVertList
    gdIndices     = makeIcosahedronIndices        newFaceList 
    gdNormals     = makeIcosahedronNormals        newVertList
    gdTangents    = makeIcosahedronTangents       newVertList


makeIcosahedronPositions :: GLfloat -> [V3 GLfloat] -> [V3 GLfloat]
makeIcosahedronPositions size vertList  = map (realToFrac size *) vertList

makeIcosahedronIndices :: (Foldable t, Foldable t1) => t (t1 b) -> [b]
makeIcosahedronIndices indexList        = concatMap toList indexList

makeIcosahedronNormals :: [V3 GLfloat] -> [V3 GLfloat]
makeIcosahedronNormals positionList     = map normalize positionList

makeIcosahedronUVs :: [V3 GLfloat] -> [V2 GLfloat]
makeIcosahedronUVs positionList = uvs
    where 
        uvs = map getUV positionList
        getUV p = 
            let u = asin (p^._x)/pi + 0.5
                v = asin (p^._y)/pi + 0.5
            in V2 u (1 - v)

makeIcosahedronTangents :: [V3 GLfloat] -> [V3 GLfloat]
makeIcosahedronTangents positionList = tangents
    where 
        tangents = map getTangent [0..length positionList]
        getTangent _ = V3 0 0 0


icosahedronGeometry :: MonadIO m => GLfloat -> GLuint -> m Geometry
icosahedronGeometry size subdivisions = geometryFromData $ icosahedronData size subdivisions

subdivide :: ([V3 GLfloat] , [V3 GLuint]) -> GLuint -> ([V3 GLfloat] , [V3 GLuint])
subdivide (vertList, faceList) 0 = (vertList , faceList)
subdivide (vertList, faceList) n = subdivide vertsAndFaces (n - 1)
  where 
    vertsAndFaces = first concat . second concat . unzip $ map getNewVerts (zip [0..] faceList)
    getNewVerts (i, face) =
      let v1 = vertList !! fromIntegral (face ^. _x)
          v2 = vertList !! fromIntegral (face ^. _y)
          v3 = vertList !! fromIntegral (face ^. _z)

          m1 = getMidpoint v1 v2
          m2 = getMidpoint v2 v3
          m3 = getMidpoint v3 v1

          newVertList = [ v1 , v2 , v3 , m1 , m2 , m3 ]
          newFaceList = [ V3 0 3 5 
                        , V3 3 1 4
                        , V3 5 3 4
                        , V3 5 4 2
                        ]
          faceListOffset = fmap (fmap (+ (i * 6))) newFaceList
          vertListNormal = fmap normalize newVertList

      in (vertListNormal, faceListOffset)


getMidpoint :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
getMidpoint v1 v2 = v2 + ((v1 - v2) / 2)
