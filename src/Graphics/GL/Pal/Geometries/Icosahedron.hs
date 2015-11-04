{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Icosahedron where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
-- import Debug.Trace
import Control.Arrow

--fI :: ( Integral a , Num b ) => a -> b
--fI = fromIntegral

icosahedronData :: GLfloat  -> GLuint -> GeometryData
icosahedronData size subdivisions = GeometryData{..}

  where

    -- t  = ( 1 + sqrt 5  ) / 2

    -- The base Array of vertices
  {-  vertList      = [ V3  (-1)   t    0 
                    , V3    1    t    0
                    , V3  (-1) (-t)   0
                    , V3    1  (-t)   0
                    , V3    0  (-1)   t
                    , V3    0    1    t
                    , V3    0  (-1) (-t)
                    , V3    0    1  (-t)
                    , V3    t    0  (-1)
                    , V3    t    0    1
                    , V3  (-t)   0  (-1)
                    , V3  (-t)   0    1
                    ]

    faceList      = [ V3   0  11   5 
                    , V3   0   5   1
                    , V3   0   1   7
                    , V3   0   7  10
                    , V3   0  10  11
                    , V3   1   5   9
                    , V3   5  11   4
                    , V3  11  10   2
                    , V3  10   7   6
                    , V3   7   1   8
                    , V3   3   9   4
                    , V3   3   4   2
                    , V3   3   2   6
                    , V3   3   6   8
                    , V3   3   8   9
                    , V3   4   9   5
                    , V3   2   4  11
                    , V3   6   2  10
                    , V3   8   6   7
                    , V3   9   8   1
                    ]
-}

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


    ( newVertList , newFaceList ) = subdivide ( vertList , faceList ) subdivisions
    --subdividedVertList  = subdivideVerts vertList
    --subdividedIndexList = subdivideIndices indexList

    gdNumVerts  = 3 * fromIntegral ( length newFaceList ) 
    gdNumPoints = 3 * fromIntegral ( length newVertList )

    gdPositions   = makeIcosahedronPositions size newVertList
    gdUVs         = makeIcosahedronUVs      newVertList
    gdIndices     = makeIcosahedronIndices newFaceList 
    gdNormals     = makeIcosahedronNormals  newVertList
    gdTangents    = makeIcosahedronTangents newVertList


makeIcosahedronPositions :: GLfloat -> [V3 GLfloat] -> [GLfloat]
makeIcosahedronPositions size vertList  = concatMap toList (map (realToFrac size *) vertList)

makeIcosahedronIndices :: (Foldable t, Foldable t1) => t (t1 b) -> [b]
makeIcosahedronIndices indexList       = concatMap toList indexList

makeIcosahedronNormals :: (Floating b, Foldable t, Metric t, Epsilon b) 
                       => [t b] -> [b]
makeIcosahedronNormals positionList     = concatMap toList (map normalize positionList) 

makeIcosahedronUVs :: [V3 GLfloat] -> [ GLfloat ]
makeIcosahedronUVs     positionList     = uvs

    where 
        uvs = concat [ getUV p | p <- positionList  ]
        getUV p = 
            let u = ((azimuth p / 2) / pi) + 0.5
                v = (inclination p / pi) + 0.5
            in [ u , 1 - v ]

azimuth :: (RealFloat a, R3 t) => t a -> a
azimuth vec = atan2 (vec ^. _z) ((-vec ^. _x))

inclination :: (RealFloat a, R3 t) => t a -> a
inclination vec = 
    atan2 
        (-vec ^. _y) 
        (sqrt (
            ((vec ^. _x) ** (vec ^. _x)) + 
            ((vec ^. _z) ** (vec ^. _z))
            )
        )

makeIcosahedronTangents :: [V3 GLfloat] -> [ GLfloat ]
makeIcosahedronTangents positionList = tangents
    where 
        tangents = concatMap getTangent [0..length positionList]
        getTangent _ = [0, 0, 0]


icosahedronGeometry :: GLfloat -> GLuint -> IO Geometry   
icosahedronGeometry size subdivisions = geometryFromData $ icosahedronData size subdivisions 

subdivide :: ([V3 GLfloat] , [V3 GLuint]) -> GLuint -> ([V3 GLfloat] , [V3 GLuint])
subdivide ( vertList , faceList ) 0 = (vertList , faceList)
subdivide ( vertList , faceList ) n = subdivide floopules (n - 1)
  where 
    floopules = first concat . second concat . unzip $ map getNewVerts (zip [0..] faceList)
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

      in (vertListNormal , faceListOffset)


getMidpoint :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
getMidpoint v1 v2 = v2 + ((v1 - v2) / 2)

{-

makePlaneUVs :: Int -> Int -> [ GLfloat ]
makePlaneUVs subdivisionsX subdivisionsY = uvs
  where

    uvs = concat [ getPoint x  y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y = toList p
      where 
        p = V2 ( fI x  / fI subdivisionsX )  ( fI y / fI subdivisionsY )
-}
